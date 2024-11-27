using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CS2C
{
    internal class Program
    {
        static void Main(string[] args)
        {
            string outputPath = null;
            string directoryPath = ".";

            for (int i = 0; i < args.Length; i++)
            {
                if (args[i] == "-o" && i + 1 < args.Length)
                {
                    outputPath = args[i + 1];
                    i++; // Skip next arg since it's the output path
                }
                else if (args[i] == "-h")
                {
                    Console.WriteLine("Usage: CS2CPU [options] [directory]");
                    Console.WriteLine("Options:");
                    Console.WriteLine("  -o <output>    Specify the output executable path");
                    Console.WriteLine("  -v             Show version information");
                    Console.WriteLine("  -h             Show help information");
                    return;
                }
                else
                {
                    directoryPath = args[i];
                }
            }

            string[] output = new Compiler().Compile(directoryPath);
        }
    }
}
