I am a Configuration of OSSubprocess. 

OSSubprocess is a software project that allows the user to spawn Operating System processes from within Pharo. The main usage of forking external OS processes is to be able to execute OS commands (.e.g cat, ls, ps, cp, etc) as well as arbitrary shell scripts (e.g. /etc/myShellScript.sh) from Pharo.

An important part of OSSubprocess is how to manage standard streams (stdin, stdout and stderr) and how to provide an API for reading and writing from them at the language level.

For more details see: https://github.com/marianopeck/OSSubprocess