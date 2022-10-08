using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.IO;
using System.Text.RegularExpressions;
using TAVCSLib;

namespace BuildTexts
{
    class Program
    {
        static int Main(string[] args)
        {
            if (args.Length == 0)
            {
                Console.WriteLine("Parametrs - directory of scenes, outfile");
                return 1;
            }

            string textcode = "";
            var res = StringProc.createPairsFromText(File.ReadAllText(args[1])) ;
                
            foreach (var dir in Directory.EnumerateDirectories(args[0])) 
            {
                foreach (var file in Directory.EnumerateFiles(dir, "*.script"))
                {
                    var fi = new FileInfo(file) ;
                    var scenecode = fi.Name.Replace(fi.Extension, "");
                    string[] lines = File.ReadAllLines(file);
                    foreach (var line in lines)
                    {
                        if (RegExHelper.getValueFromRegEx(line, @"'\$(.*?)'", out textcode))
                        {
                            string key = scenecode + "_"+textcode;
                            if (!res.ContainsKey(key)) res.Add(key, "new");
                        }
                    }
                }                                
            }

            var sorted = res.Keys.ToList();
            sorted.Sort();
            var stm = new StreamWriter(args[1]);
            foreach (var key in sorted)
                stm.WriteLine("{0}={1}", key, res[key]);
            stm.Close();

            return 0;
        }
    }
}
