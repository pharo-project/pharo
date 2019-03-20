"
Clap is a parser for command-line arguments.

The general process in Clap has three steps:

1. We start from a static specification of a command, its parameters, and their behavior.

   The command specification is recursively composed from instances of ClapCommand (a named command or sub-command with flags and positional parameters), ClapFlag (a keyword representing an option or naming a parameter) and ClapPositional (a value passed in sequence).

2. At invocation time, we build a context to match the specification against an actual sequence of arguments coming from the shell.

3. The resulting context is an activation of the command, which can be evaluated.
 
   The activation is a structured record of the parameters that matched the invocation and how; at its root, the context provides access to external resources such as the standard I/O streams.

"
Class {
	#name : #ManifestClapCore,
	#superclass : #PackageManifest,
	#category : #'Clap-Core-Manifest'
}
