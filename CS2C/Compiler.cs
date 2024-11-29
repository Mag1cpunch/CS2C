using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;

namespace CS2C
{
    public class MethodInfo
    {
        public MethodDeclarationSyntax Declaration { get; set; }
        public string MethodName { get; set; }
        public string UniqueMethodName { get; set; }
        public List<string> ParameterTypes { get; set; }
        public bool IsStatic { get; set; }
    }

    public class ClassInfo
    {
        public string ClassName { get; set; }
        public List<(string Type, string Name)> InstanceFields { get; set; } = new List<(string, string)>();
        public List<MethodDeclarationSyntax> Methods { get; set; } = new List<MethodDeclarationSyntax>();
    }

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

        private Dictionary<string, string> MethodMappings;

        private List<string> Output;
        private int IndentLevel = 0;

        private Dictionary<string, int> MethodOverloadCounts;
        private Dictionary<MethodDeclarationSyntax, int> MethodOverloadNumbers;

        public List<ClassInfo> Classes { get; set; } = new List<ClassInfo>();

        private Dictionary<string, string> ExternalMethodMappings = new Dictionary<string, string>()
        {
            { "System.Console.WriteLine(char*)", "printf" },
        };

        private readonly string CompilationTimestamp = DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss");


        public Compiler()
        {
            Includes = new();
            NamespaceDeclarations = new();
            VariableDeclarations = new();
            MethodDeclarations = new();
            ClassDeclarations = new();

            CurrentNamespace = string.Empty;
            CurrentClass = string.Empty;
            CurrentMethod = string.Empty;
            Output = new();

            MethodOverloadCounts = new();
            MethodOverloadNumbers = new();

            MethodMappings = new();
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

            Output.Clear();

            Includes.Clear();

            Output.AddRange(Includes);

            ProcessClasses(syntaxTrees);
            GenerateStructs();
            GenerateMethods();

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

        private void GenerateStructs()
        {
            foreach (var classInfo in Classes)
            {
                string structName = MapType(classInfo.ClassName);

                WriteLine($"typedef struct {structName}");
                WriteLine("{");
                IndentLevel++;

                foreach (var field in classInfo.InstanceFields)
                {
                    var variableType = field.Type;
                    var variableName = field.Name;

                    WriteLine($"{variableType} {variableName};");
                }

                IndentLevel--;
                WriteLine($"}} {structName};");
                WriteLine(string.Empty);
            }
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

                        var classInfo = new ClassInfo
                        {
                            ClassName = CurrentClass,
                        };

                        ProcessClassFields(@class, classInfo);
                        ProcessMethods(@class, classInfo);

                        Classes.Add(classInfo);
                    }
                }
            }
        }

        private void ProcessClassFields(ClassDeclarationSyntax classSyntax, ClassInfo classInfo)
        {
            var fieldDeclarations = classSyntax.Members.OfType<FieldDeclarationSyntax>();
            foreach (var field in fieldDeclarations)
            {
                var variableDeclaration = field.Declaration;
                bool isStatic = field.Modifiers.Any(SyntaxKind.StaticKeyword);

                foreach (var variableDeclarator in variableDeclaration.Variables)
                {
                    var variableName = variableDeclarator.Identifier.Text;
                    var variableType = MapType(variableDeclaration.Type.ToString());

                    VariableDeclarations.Add(variableDeclarator);

                    if (isStatic)
                    {
                        WriteLine($"{variableType} {CurrentNamespace}_{classInfo.ClassName}_{variableName};");

                        if (variableDeclarator.Initializer != null)
                        {
                            if (variableDeclarator.Initializer.Value.ToString().Length > 0)
                                Write($" = {variableDeclarator.Initializer.Value}");
                        }

                        Write(";");

                        Console.WriteLine($"Found static variable {CurrentNamespace}_{classInfo.ClassName}_{variableName}, Value: {variableDeclarator.Initializer?.Value.ToString()}");
                    }
                    else
                    {
                        classInfo.InstanceFields.Add((variableType, variableName));
                    }
                }
            }
        }


        private void ProcessMethods(ClassDeclarationSyntax classSyntax, ClassInfo classInfo)
        {
            var methodDeclarations = classSyntax.DescendantNodes().OfType<MethodDeclarationSyntax>();

            foreach (var method in methodDeclarations)
            {
                var methodName = method.Identifier.Text;
                if (methodName.ToLower() == "asm")
                {
                    continue;
                }

                classInfo.Methods.Add(method);

                var methodType = MapType(method.ReturnType.ToString());

                var parameterTypes = method.ParameterList.Parameters
                    .Select(p => MapType(p.Type?.ToString() ?? "void"))
                    .ToList();

                bool isStatic = method.Modifiers.Any(SyntaxKind.StaticKeyword);
                var uniqueMethodKey = $"{CurrentNamespace}_{CurrentClass}_{methodName}";

                if (!MethodOverloadCounts.ContainsKey(uniqueMethodKey))
                {
                    MethodOverloadCounts[uniqueMethodKey] = 0;
                }
                else
                {
                    MethodOverloadCounts[uniqueMethodKey]++;
                }

                int overloadNumber = MethodOverloadCounts[uniqueMethodKey];
                MethodOverloadNumbers[method] = overloadNumber;

                string fullMethodName = $"{uniqueMethodKey}{overloadNumber}";

                CurrentMethod = fullMethodName;

                string methodSignature;

                if (isStatic)
                {
                    methodSignature = $"{CurrentClass}.{methodName}({string.Join(", ", parameterTypes)})";
                }
                else
                {
                    parameterTypes.Insert(0, $"{MapType(CurrentClass)}*");
                    methodSignature = $"{CurrentClass}.{methodName}({string.Join(", ", parameterTypes)})";
                }

                MethodMappings[methodSignature] = fullMethodName;

                WriteLine($"{methodType} {fullMethodName}(");
                ProcessMethodArgs(method.ParameterList, isStatic);
                Write(");");

                Console.WriteLine($"Generated method: {fullMethodName}, Parameters: ({string.Join(", ", parameterTypes)})");
                MethodDeclarations.Add(method);
            }
            CurrentMethod = string.Empty;
        }

        private void ProcessMethodArgs(ParameterListSyntax args, bool isStatic)
        {
            var parameters = new List<string>();

            if (!isStatic)
                parameters.Add($"{MapType(CurrentClass)}* this");

            foreach (var parameter in args.Parameters)
            {
                string argName = parameter.Identifier.Text;
                string argType = MapType(parameter.Type?.ToString() ?? "void");
                parameters.Add($"{argType} {argName}");
            }

            Write(string.Join(", ", parameters));
        }


        private void GenerateMethods()
        {
            WriteLine(string.Empty);
            foreach (var classInfo in Classes)
            {
                CurrentClass = classInfo.ClassName;

                foreach (var method in classInfo.Methods)
                {
                    var methodName = method.Identifier.Text;
                    var methodType = MapType(method.ReturnType.ToString());
                    var uniqueMethodKey = $"{CurrentNamespace}_{CurrentClass}_{methodName}";

                    if (!MethodOverloadNumbers.TryGetValue(method, out int overloadNumber))
                    {
                        Console.WriteLine($"Warning: Overload number for method '{methodName}' not found.");
                        overloadNumber = 0;
                    }

                    string fullMethodName = $"{uniqueMethodKey}{overloadNumber}";

                    CurrentMethod = fullMethodName;

                    bool isStatic = method.Modifiers.Any(SyntaxKind.StaticKeyword);

                    WriteLine($"{methodType} {fullMethodName}(");
                    ProcessMethodArgs(method.ParameterList, isStatic);
                    Write(")");
                    WriteLine("{");
                    IndentLevel++;
                    WriteLine($"// {method.ReturnType.ToString()} {methodName}({method.ParameterList.Parameters.ToString()});");

                    var tree = method.SyntaxTree;

                    var references = AppDomain.CurrentDomain.GetAssemblies()
                        .Where(a => !a.IsDynamic && !string.IsNullOrEmpty(a.Location))
                        .Select(a => MetadataReference.CreateFromFile(a.Location))
                        .ToList();

                    var compilation = CSharpCompilation.Create("Compilation")
                        .AddReferences(references)
                        .AddSyntaxTrees(tree);

                    var semanticModel = compilation.GetSemanticModel(tree);
                    Visit(method.Body, semanticModel);
                    IndentLevel--;
                    WriteLine("}");
                }
            }
            WriteLine(string.Empty);
        }

        private void ProcessInvocation(InvocationExpressionSyntax invocation, SemanticModel semanticModel)
        {
            string methodName;
            string objectName = null;
            bool isStatic = false;
            string className = null;

            ExpressionSyntax expression = invocation.Expression;

            if (expression is IdentifierNameSyntax identifierName)
            {
                methodName = identifierName.Identifier.Text;

                if (methodName == "__signature__")
                {
                    HandleSignatureInvocation(invocation, semanticModel);
                    return;
                }
                else if (methodName == "__typeof__")
                {
                    HandleTypeOfInvocation(invocation, semanticModel);
                    return;
                }
                else if (methodName == "__sizeof__")
                {
                    HandleSizeOfInvocation(invocation, semanticModel);
                    return;
                }
                else if (methodName == "__traits__")
                {
                    HandleTraitsInvocation(invocation, semanticModel);
                    return;
                }
                else if (methodName == "__is_defined__")
                {
                    HandleIsDefinedInvocation(invocation, semanticModel);
                    return;
                }

                var symbolInfo = semanticModel.GetSymbolInfo(identifierName);
                if (symbolInfo.Symbol is IMethodSymbol methodSymbol)
                {
                    className = methodSymbol.ContainingType.ToString();
                    isStatic = methodSymbol.IsStatic;
                }
                else
                {
                    className = CurrentClass;
                    isStatic = IsMethodStatic(CurrentClass, methodName);
                }
            }
            else if (expression is MemberAccessExpressionSyntax memberAccess)
            {
                methodName = memberAccess.Name.Identifier.Text;

                var typeInfo = semanticModel.GetTypeInfo(memberAccess.Expression);

                if (typeInfo.Type != null)
                    className = typeInfo.Type.ToString();
                else
                    className = memberAccess.Expression.ToString();

                var symbolInfo = semanticModel.GetSymbolInfo(memberAccess.Name);
                if (symbolInfo.Symbol is IMethodSymbol methodSymbol)
                    isStatic = methodSymbol.IsStatic;
                else
                    isStatic = true;
            }
            else
                methodName = expression.ToString();

            if (className == null)
                className = CurrentClass;

            List<string> arguments = new List<string>();
            List<string> argumentTypes = new List<string>();

            foreach (var arg in invocation.ArgumentList.Arguments)
            {
                var argExpression = arg.Expression;

                if (argExpression is InvocationExpressionSyntax innerInvocation)
                {
                    var innerExpression = innerInvocation.Expression;
                    if (innerExpression is IdentifierNameSyntax innerIdentifier && innerIdentifier.Identifier.Text == "__signature__")
                    {
                        var signature = GetSignatureFromInvocation(innerInvocation, semanticModel);
                        arguments.Add($"\"{signature}\"");
                        argumentTypes.Add("char*");
                        continue;
                    }
                }

                var typeInfo = semanticModel.GetTypeInfo(argExpression);
                string argType = MapType(typeInfo.Type?.ToString() ?? "unknown");
                argumentTypes.Add(argType);

                if (argExpression is LiteralExpressionSyntax literal && literal.IsKind(SyntaxKind.StringLiteralExpression))
                    arguments.Add($"\"{literal.Token.ValueText}\"");
                else
                    arguments.Add(argExpression.ToString());
            }

            if (!isStatic && objectName == null)
                objectName = "this";

            if (!isStatic && objectName != null)
                arguments.Insert(0, objectName);

            string methodSignature = $"{className}.{methodName}({string.Join(", ", argumentTypes)})";

            if (MethodMappings.TryGetValue(methodSignature, out string fullMethodName))
            {
                string functionCall = $"{fullMethodName}({string.Join(", ", arguments)});";
                WriteLine(functionCall);
                Console.WriteLine($"Detected function call: {functionCall}");
            }
            else
            {
                Console.WriteLine($"Method '{methodSignature}' not found in mappings.");
                HandleExternalMethodInvocation(methodSignature, methodName, arguments, argumentTypes, isStatic);
            }
        }

        private void HandleIsDefinedInvocation(InvocationExpressionSyntax invocation, SemanticModel semanticModel)
        {
            var argument = invocation.ArgumentList.Arguments.FirstOrDefault();
            if (argument == null)
            {
                WriteLine("// Error: __is_defined__ requires a symbol as an argument");
                return;
            }

            var expr = argument.Expression;
            var symbolInfo = semanticModel.GetSymbolInfo(expr);
            bool isDefined = symbolInfo.Symbol != null;

            WriteLine(isDefined ? "1" : "0");
        }

        private void HandleTraitsInvocation(InvocationExpressionSyntax invocation, SemanticModel semanticModel)
        {
            var argument = invocation.ArgumentList.Arguments.FirstOrDefault();
            if (argument == null)
            {
                WriteLine("// Error: __traits__ requires an expression as an argument");
                return;
            }

            var expr = argument.Expression;
            var typeInfo = semanticModel.GetTypeInfo(expr);
            var typeSymbol = typeInfo.Type;

            if (typeSymbol != null)
            {
                var traits = new List<string>();

                if (typeSymbol.IsReferenceType)
                    traits.Add("ReferenceType");
                if (typeSymbol.IsValueType)
                    traits.Add("ValueType");
                if (typeSymbol.AllInterfaces.Any(i => i.Name == "IDisposable"))
                    traits.Add("Implements IDisposable");

                WriteLine($"\"{string.Join(", ", traits)}\";");
            }
            else
            {
                WriteLine($"// Error: Unable to determine traits of {expr}");
            }
        }

        private void HandleSizeOfInvocation(InvocationExpressionSyntax invocation, SemanticModel semanticModel)
        {
            var argument = invocation.ArgumentList.Arguments.FirstOrDefault();
            if (argument == null)
            {
                WriteLine("// Error: __sizeof__ requires a type as an argument");
                return;
            }

            var expr = argument.Expression;

            var typeInfo = semanticModel.GetTypeInfo(expr);
            var typeSymbol = typeInfo.Type;

            if (typeSymbol != null)
            {
                string cType = MapType(typeSymbol.ToString());

                WriteLine($"sizeof({cType});");
            }
            else
            {
                WriteLine($"// Error: Unable to determine size of type {expr}");
            }
        }


        private void HandleTypeOfInvocation(InvocationExpressionSyntax invocation, SemanticModel semanticModel)
        {
            var argument = invocation.ArgumentList.Arguments.FirstOrDefault();
            if (argument == null)
            {
                WriteLine("// Error: __typeof__ requires an expression as an argument");
                return;
            }

            var expr = argument.Expression;
            var typeInfo = semanticModel.GetTypeInfo(expr);
            string typeName = MapType(typeInfo.Type?.ToString() ?? "unknown");
            WriteLine($"\"{typeName}\";");
        }


        private string GetSignatureFromInvocation(InvocationExpressionSyntax invocation, SemanticModel semanticModel)
        {
            var argument = invocation.ArgumentList.Arguments.FirstOrDefault();
            if (argument == null)
            {
                return "/* Error: __signature__ requires a function name as an argument */";
            }

            var expr = argument.Expression;

            var symbolInfo = semanticModel.GetSymbolInfo(expr);
            ISymbol symbol = symbolInfo.Symbol;

            if (symbol == null && symbolInfo.CandidateSymbols.Length > 0)
            {
                symbol = symbolInfo.CandidateSymbols[0];
            }

            if (symbol is IMethodSymbol methodSymbol)
            {
                return GetMethodSignature(methodSymbol);
            }
            else
            {
                return $"/* Error: Unable to retrieve signature for {expr} */";
            }
        }


        private void HandleSignatureInvocation(InvocationExpressionSyntax invocation, SemanticModel semanticModel)
        {
            var argument = invocation.ArgumentList.Arguments.FirstOrDefault();
            if (argument == null)
            {
                WriteLine("// Error: __signature__ requires a function name as an argument");
                return;
            }

            var expr = argument.Expression;

            var symbolInfo = semanticModel.GetSymbolInfo(expr);
            ISymbol symbol = symbolInfo.Symbol;

            if (symbol == null && symbolInfo.CandidateSymbols.Length > 0)
            {
                symbol = symbolInfo.CandidateSymbols[0];
            }

            if (symbol is IMethodSymbol methodSymbol)
            {
                string signature = GetMethodSignature(methodSymbol);
                WriteLine($"\"{signature}\"");
            }
            else
            {
                WriteLine($"// Error: Unable to retrieve signature for {expr}");
            }
        }

        private string GetMethodSignature(IMethodSymbol methodSymbol)
        {
            var returnType = MapType(methodSymbol.ReturnType.ToString());
            var methodName = methodSymbol.Name;
            var parameters = methodSymbol.Parameters.Select(p =>
            {
                var paramType = MapType(p.Type.ToString());
                var paramName = p.Name;
                return $"{paramType} {paramName}";
            });

            return $"{returnType} {methodName}({string.Join(", ", parameters)})";
        }



        private void HandleExternalMethodInvocation(string methodSignature, string methodName, List<string> arguments, List<string> argumentTypes, bool isStatic)
        {
            if (ExternalMethodMappings.TryGetValue(methodSignature, out string externalFunctionName))
            {
                if (!isStatic && arguments.Count > 0 && arguments[0] == "this")
                {
                    arguments.RemoveAt(0);
                }

                string functionCall = $"{externalFunctionName}({string.Join(", ", arguments)});";
                WriteLine(functionCall);
                Console.WriteLine($"Mapped external function call: {functionCall}");
            }
            else
            {
                WriteLine($"// TODO: Implement handling for external method '{methodSignature}'");
            }
        }


        private bool IsMethodStatic(string className, string methodName)
        {
            var method = MethodDeclarations.FirstOrDefault(m =>
                m.Identifier.Text == methodName &&
                m.Parent is ClassDeclarationSyntax classDecl &&
                classDecl.Identifier.Text == className);

            if (method != null)
            {
                return method.Modifiers.Any(SyntaxKind.StaticKeyword);
            }

            return true;
        }

        private string GetFullMethodName(ExpressionSyntax expression, SeparatedSyntaxList<ArgumentSyntax> arguments)
        {
            string methodName;
            string objectName = null;

            if (expression is IdentifierNameSyntax identifierName)
            {
                methodName = identifierName.Identifier.Text;
            }
            else if (expression is MemberAccessExpressionSyntax memberAccess)
            {
                objectName = memberAccess.Expression.ToString(); // The instance variable
                methodName = memberAccess.Name.Identifier.Text;
            }
            else
            {
                methodName = expression.ToString();
            }

            var argumentTypes = arguments.Select(arg => "/* Resolve argument type */").ToList();

            string methodSignature = $"{CurrentClass}.{methodName}({string.Join(", ", argumentTypes)})";

            if (MethodMappings.TryGetValue(methodSignature, out string fullMethodName))
            {
                return fullMethodName;
            }

            return null;
        }

        private string GetMethodName(ExpressionSyntax expression)
        {
            if (expression is IdentifierNameSyntax identifierName)
            {
                return identifierName.Identifier.Text;
            }
            else if (expression is MemberAccessExpressionSyntax memberAccess)
            {
                string objectName = memberAccess.Expression.ToString();
                string methodName = memberAccess.Name.Identifier.Text;
                return $"{objectName}_{methodName}";
            }
            else
            {
                return expression.ToString();
            }
        }

        private void Visit(SyntaxNode node, SemanticModel semanticModel)
        {
            if (node == null)
                return;

            switch (node.Kind())
            {
                case SyntaxKind.IfStatement:
                    var ifStatement = (IfStatementSyntax)node;
                    Console.WriteLine("IF STATEMENT:");

                    WriteLine($"if ({ifStatement.Condition})");
                    WriteLine("{");
                    IndentLevel++;

                    Visit(ifStatement.Statement, semanticModel);

                    IndentLevel--;
                    WriteLine("}");

                    if (ifStatement.Else != null)
                    {
                        WriteLine("else");
                        WriteLine("{");
                        IndentLevel++;
                        Visit(ifStatement.Else.Statement, semanticModel);
                        IndentLevel--;
                        WriteLine("}");
                    }
                    break;
                case SyntaxKind.WhileStatement:
                    var whileStatement = (WhileStatementSyntax)node;
                    Console.WriteLine("WHILE STATEMENT:");
                    WriteLine($"while ({whileStatement.Condition})");
                    WriteLine("{");
                    IndentLevel++;
                    Visit(whileStatement.Statement, semanticModel);
                    IndentLevel--;
                    WriteLine("}");
                    break;
                case SyntaxKind.ForStatement:
                    var forStatement = (ForStatementSyntax)node;
                    Console.WriteLine("FOR STATEMENT:");
                    WriteLine($"for ({forStatement.Declaration}; {forStatement.Condition}; {forStatement.Incrementors})");
                    WriteLine("{");
                    IndentLevel++;
                    Visit(forStatement.Statement, semanticModel);
                    IndentLevel--;
                    WriteLine("}");
                    break;
                case SyntaxKind.IdentifierName:
                    var identifier = (IdentifierNameSyntax)node;
                    if (identifier.Identifier.Text == "__line__")
                    {
                        int lineNumber = node.GetLocation().GetLineSpan().StartLinePosition.Line + 1;
                        WriteLine($"{lineNumber}");
                    }
                    else if (identifier.Identifier.Text == "__file__")
                    {
                        string fileName = Path.GetFileName(node.SyntaxTree.FilePath);
                        WriteLine($"\"{fileName}\"");
                    }
                    else if (identifier.Identifier.Text == "__timestamp__")
                    {
                        WriteLine($"\"{CompilationTimestamp}\"");
                    }
                    else if (identifier.Identifier.Text == "__func__")
                    {
                        WriteLine($"\"{CurrentMethod}\"");
                    }
                    else if (identifier.Identifier.Text == "__namespace__")
                    {
                        WriteLine($"\"{CurrentNamespace}\"");
                    }
                    else if (identifier.Identifier.Text == "__class__")
                    {
                        WriteLine($"\"{CurrentClass}\"");
                    }
                    else
                    {
                        WriteLine(identifier.Identifier.Text);
                    }
                    Console.WriteLine($"IDENTIFIER: {identifier}");
                    break;
                case SyntaxKind.LocalDeclarationStatement:
                    var declaration = (LocalDeclarationStatementSyntax)node;
                    foreach (var variable in declaration.Declaration.Variables)
                    {
                        string varType = MapType(declaration.Declaration.Type.ToString());
                        string varName = variable.Identifier.Text;
                        WriteLine($"{varType} {varName}");
                        if (variable.Initializer != null)
                        {
                            Write($" = {variable.Initializer.Value}");
                        }
                        Write(";");
                    }
                    break;
                case SyntaxKind.InvocationExpression:
                    var invocation = (InvocationExpressionSyntax)node;
                    ProcessInvocation(invocation, semanticModel);
                    break;
                case SyntaxKind.ThisExpression:
                    Write("this");
                    break;

                case SyntaxKind.ReturnStatement:
                    var returnStatement = (ReturnStatementSyntax)node;
                    WriteLine($"return {returnStatement.Expression};");
                    break;

                default:
                    foreach (var child in node.ChildNodes())
                    {
                        Visit(child, semanticModel);
                    }
                    break;
            }
        }

        private string MapMethod(string method)
        {
            if (MethodMappings.ContainsKey(method))
            {
                return MethodMappings[method];
            }
            return method;
        }

        private string MapType(string type)
        {
            if (type == null) return "void";

            if (type.Contains("."))
            {
                type = type.Split('.').Last();
            }

            switch (type)
            {
                case "Int32":
                case "int":
                    return "int";
                case "String":
                case "string":
                    return "char*";
                case "Boolean":
                case "bool":
                    return "bool";
                case "Single":
                case "float":
                    return "float";
                case "Double":
                case "double":
                    return "double";
                case "Decimal":
                case "decimal":
                    return "decimal";
                case "Int64":
                case "long":
                    return "long";
                case "Int16":
                case "short":
                    return "short";
                case "Byte":
                case "byte":
                    return "unsigned char";
                case "Char":
                case "char":
                    return "char";
                case "SByte":
                case "sbyte":
                    return "signed char";
                case "UInt32":
                case "uint":
                    return "unsigned int";
                case "UInt16":
                case "ushort":
                    return "unsigned short";
                case "UInt64":
                case "ulong":
                    return "unsigned long";
                case "void":
                    return "void";
                case "Object":
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
                    if (type.EndsWith("*"))
                    {
                        int dimensions = type.Count(c => c == '*');
                        return $"{MapType(type.Replace("*", ""))}{new string('*', dimensions)}";
                    }
                    return type;
            }
        }
    }
}
