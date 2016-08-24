pages
browserEnvironments
	^HelpTopic
		title: 'RBBrowserEnvironment'
		contents: 
'
The first and main use for browser environments are to restrict the namespace in which a refactoring operation is applied. For example, if you want to rename a method and and update all senders of this method, but only in a certain package, you can create a RBNamespace from a scoped ''view'' of the classes from the whole system. Only the classes in this restricted environment are affected by the transformation.

In the mean time other tools are using this environment classes as well. Finder, MessageBrowser or the SystemBrowser can work with a scoped environment to show and operate only on classes and methods in this environment.

There are different subclasses of RBBrowserEnvironment for the different kind of ''scopes''. 

RBClassEnvironment - only show classes/methods from a set of classes.
RBPackageEnvironment - only show classes / packages / methods from a set of packages.
RBSelectorEnvironment - only show classes / methods from a set of selector names.
(see the list of RBEnvironment(subclasses) pages in this book).

Instead of directly using the different subclasses for a scoped view, the base class RBBrowserEnvironment can act as a factory for creating restricted environments. See the methods in its ''environments''-protocol, on how to create the different environments.

You start with a default environment containing all classes from the system and create a new scoped environment by calling the appropriate method.

For example, creating an environment for all classes in package ''Kernel'':

RBBrowserEnvironment new forPackageNames:{''Kernel''}.

You can query the environment just like you for Smalltalk globals
|env|
env := RBBrowserEnvironment new forPackageNames:{''Kernel''}.
env allClasses "-> a list of all classes in package Kernel"

or open a browser
env browse "-> starts Nautilus showing only this package"

and you can further restrict this package environment by calling one of the other factory methods:

env class "-> a RBPackageEnvironment"
(env implementorsOf:#collect:) class "->  RBSelectorEnvironment"

Another way to combine or further restrict environments is to use boolean operations and, not or or.

|implDrawOn callsDrawOn implAndCalls |
callsDrawOn := RBBrowserEnvironment new referencesTo: #drawOn:.
implDrawOn :=  RBBrowserEnvironment new implementorsOf: #drawOn:.
"create an ''anded''-environment"
implAndCalls := callsDrawOn & implDrawOn.
"collect all message and open a MessageBrowser"
MessageBrowser browse: implAndCalls methods.

This opens a MessageBrowser on all methods in the system that implement #drawOn: and calls drawOn:.

|implPrintOn notImplPrintOn |
implPrintOn := RBBrowserEnvironment new implementorsOf: #printOn:.
"create a ''not''-environment"
notImplPrintOn := implPrintOn not.
implPrintOn includesClass: Object. "-> true"
notImplPrintOn includesClass: Object. "-> false"

classes implementing #printOn: are not in the ''not''-environment.

A more generic way to create an environment by giving an explicit ''test''-block to select methods for this environment:

|implementedByMe|
implementedByMe := RBBrowserEnvironment new selectMethods:[:m | m author = Author fullName ].
implementedByMe browse.
 
This opens (may be slow) a browser with all classes with methods having my (current Author) name for its current methods version author stamp.
'