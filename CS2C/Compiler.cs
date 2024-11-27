using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Security.Claims;
using System.Text;
using System.Threading.Tasks;

namespace CS2C
{
    public class Compiler
    {
        public List<string> Includes;
        public List<NamespaceDeclarationSyntax> NamespaceDeclarations;
        public List<VariableDeclaratorSyntax> VariableDeclarations;
        public List<MethodDeclarationSyntax> MethodDeclarations;
        public List<ClassDeclarationSyntax> ClassDeclarations;
        public string CurrentNamespace;
        public string CurrentClass;
        public string CurrentMethod;

        private List<string> Output;
        private int IndentLevel = 0;


        public Compiler()
        {
            Includes = new List<string>();
            NamespaceDeclarations = new();
            VariableDeclarations = new();
            MethodDeclarations = new();
            ClassDeclarations = new();

            CurrentNamespace = string.Empty;
            CurrentClass = string.Empty;
            CurrentMethod = string.Empty;

            Output = new();
        }

        public string[] Compile(string projectDir)
        {

            var csFiles = Directory.GetFiles(projectDir, "*.cs", SearchOption.AllDirectories);

            List<SyntaxTree> syntaxTrees = new List<SyntaxTree>();
            foreach (var file in csFiles)
            {
                string code = File.ReadAllText(file);
                SyntaxTree syntaxTree = CSharpSyntaxTree.ParseText(code);
                syntaxTrees.Add(syntaxTree);
            }

            // code generation step

            Output.Clear();

            Includes.Clear();

            Output.AddRange(Includes);

            ProcessClasses(syntaxTrees);

            Console.WriteLine($"Code: {string.Join("\n", Output)}");

            return Output.ToArray();
        }

        private void WriteLine(string str)
        {
            Output.Add(new string(' ', IndentLevel * 4) + str);
        }

        private void Write(string str)
        {
            Output[Output.Count - 1] += str;
        }

        private void ProcessClasses(List<SyntaxTree> syntaxTrees)
        {
            foreach (SyntaxTree tree in syntaxTrees)
            {
                var root = tree.GetRoot();
                var namespaces = root.DescendantNodes().OfType<NamespaceDeclarationSyntax>();
                foreach (var @namespace in namespaces)
                {
                    NamespaceDeclarations.Add(@namespace);
                    CurrentNamespace = @namespace.Name.ToString();
                    var classes = @namespace.DescendantNodes().OfType<ClassDeclarationSyntax>();
                    foreach (var @class in classes)
                    {
                        if (classes.Count(c => c.Identifier.Text == @class.Identifier.Text && c.Parent == @class.Parent) > 1)
                        {
                            throw new Exception($"Class \"{@class.Identifier.Text}\" already exists in this namespace, namespace: {@namespace.Name.ToString()}");
                        }
                        ClassDeclarations.Add(@class);
                        CurrentClass = @class.Identifier.Text;

                        ProcessClassFields(@class);
                        ProcessMethods(@class);
                        GenerateMethods();
                    }
                }
            }
        }

        private void ProcessClassFields(ClassDeclarationSyntax classSyntax)
        {
            var fieldDeclarations = classSyntax.DescendantNodes().OfType<FieldDeclarationSyntax>();
            foreach (var field in fieldDeclarations)
            {
                var variableDeclaration = field.Declaration;
                foreach (var variableDeclarator in variableDeclaration.Variables)
                {
                    var variableName = variableDeclarator.Identifier.Text;
                    var variableType = MapType(variableDeclaration.Type.ToString());

                    if (fieldDeclarations.Count(v => v.Declaration.Variables.Any(var => var.Identifier.Text == variableName) && v.Declaration.Type.ToString() == variableType && v.Parent == field.Parent) > 1)
                    {
                        throw new Exception($"Variable \"{variableName}\" of type \"{variableType}\" already exists in this class, class: {CurrentClass}");
                    }

                    VariableDeclarations.Add(variableDeclarator);

                    WriteLine($"{variableType} {CurrentNamespace}_{CurrentClass}_{variableName}");

                    if (variableDeclarator.Initializer != null)
                    {
                        if (variableDeclarator.Initializer.Value.ToString().Length > 0)
                            Write($" = {variableDeclarator.Initializer.Value}");
                    }

                    Write(";");

                    Console.WriteLine($"Found variable {CurrentNamespace}_{CurrentClass}_{variableName}, Value: {variableDeclarator.Initializer?.Value.ToString()}");
                }
            }
        }

        private void ProcessMethods(ClassDeclarationSyntax classSyntax)
        {
            var methodDeclarations = classSyntax.DescendantNodes().OfType<MethodDeclarationSyntax>();
            foreach (var method in methodDeclarations)
            {
                var methodName = method.Identifier.Text;
                var methodType = MapType(method.ReturnType.ToString());
                CurrentMethod = methodName;


                if (methodDeclarations.Count(v => v.Identifier.Text == methodName && v.ReturnType.ToString() == methodType && v.Parent == method.Parent) > 1)
                {
                    throw new Exception($"Method \"{methodName}\" of type \"{methodType}\" already exists in this class, class: {CurrentClass}");
                }
                MethodDeclarations.Add(method);
                WriteLine($"{methodType} {CurrentNamespace}_{CurrentClass}_{methodName}(");
                ProcessMethodArgs(method.ParameterList);
                Write(");");
                Console.WriteLine($"Found method {CurrentNamespace}_{CurrentClass}_{methodName}, Return Type: {methodType}");
            }
            CurrentMethod = string.Empty;
        }

        private void ProcessMethodArgs(ParameterListSyntax args)
        {
            for (int i = 0; i < args.Parameters.Count; i++)
            {
                string argName = args.Parameters[i].Identifier.Text;
                string argType = MapType(args.Parameters[i].Type.ToString());
                Write($"{argType} {CurrentNamespace}_{CurrentClass}_{CurrentMethod}_{argName}");
            } 
        }

        private void GenerateMethods()
        {
            WriteLine(string.Empty);
            foreach (var method in MethodDeclarations)
            {
                WriteLine($"{MapType(method.ReturnType.ToString())} {CurrentNamespace}_{CurrentClass}_{method.Identifier.Text}()");
                WriteLine("{");
                IndentLevel++;
                WriteLine("// Method Body");
                Visit(method.Body.SyntaxTree.GetRoot());
                IndentLevel--;
                WriteLine("}");
            }
            WriteLine(string.Empty);
        }

        private void Visit(SyntaxNode node)
        {
            foreach (var childNode in node.ChildNodes())
            {
                switch (childNode.Kind())
                {
                    case SyntaxKind.VariableDeclaration:
                        //var variableDeclaration = (VariableDeclarationSyntax)childNode;
                        //foreach (var variableDeclarator in variableDeclaration.Variables)
                        //{
                        //    var variableName = variableDeclarator.Identifier.Text;
                        //    var variableType = MapType(variableDeclaration.Type.ToString());
                        //    WriteLine($"{variableType} {CurrentNamespace}_{CurrentClass}_{CurrentMethod}_{variableName}");
                        //    if (variableDeclarator.Initializer != null)
                        //    {
                        //        if (variableDeclarator.Initializer.Value.ToString().Length > 0)
                        //            Write($" = {variableDeclarator.Initializer.Value}");
                        //    }
                        //    Write(";");
                        //}
                        break;
                    case SyntaxKind.IdentifierName:
                        Write(childNode.ToString());
                        break;
                    case SyntaxKind.StringLiteralExpression:
                        Write("\"" + childNode.ToString() + "\"");
                        break;
                    case SyntaxKind.NumericLiteralExpression:
                        Write(childNode.ToString());
                        break;
                    //case SyntaxKind.:
                    //    Write(childNode.ToString());
                    //    break;
                    case SyntaxKind.InvocationExpression:
                        Write(childNode.ToString());
                        break;
                    case SyntaxKind.ObjectCreationExpression:
                        Write(childNode.ToString());
                        break;
                    case SyntaxKind.SimpleMemberAccessExpression:
                        Write(childNode.ToString());
                        break;
                    case SyntaxKind.Block:
                        WriteLine("{");
                        IndentLevel++;
                        Visit(childNode);
                        IndentLevel--;
                        WriteLine("}");
                        break;
                    default:
                        Visit(childNode);
                        break;
                }
            }
        }


        private string MapType(string type)
        {
            switch (type)
            {
                case "int":
                    return "int";
                case "string":
                    return "char*";
                case "bool":
                    return "bool";
                case "float":
                    return "float";
                case "double":
                    return "double";
                case "decimal":
                    return "decimal";
                case "long":
                    return "long";
                case "short":
                    return "short";
                case "byte":
                    return "unsigned char";
                case "char":
                    return "char";
                case "sbyte":
                    return "signed char";
                case "uint":
                    return "unsigned int";
                case "ushort":
                    return "unsigned short";
                case "ulong":
                    return "unsigned long";
                case "void":
                    return "void";
                case "object":
                    return "void*";
                case "var":
                    throw new Exception("Var type is not supported");
                case "dynamic":
                    throw new Exception("Dynamic type is not supported");
                default:
                    if (type.Contains("[]"))
                    {
                        int dimensions = type.Count(c => c == '[');
                        return $"{MapType(type.Replace("[]", ""))}{new string('*', dimensions)}";
                    }
                    return type;
            }
        }
    }
}
