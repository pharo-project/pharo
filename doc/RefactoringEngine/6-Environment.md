## Using environment

The first and main use for browser environments are to restrict the namespace in which a refactoring operation is applied. 
For example, if you want to rename a method and update all senders of this method, but only in a certain package, you can create a `RBNamespace` from a scoped 'view' of the classes from the whole system. Only the classes in this restricted environment are affected by the transformation.

In the meantime other tools are using this environment classes as well. Finder, MessageBrowser or the SystemBrowser can work with a scoped environment to show and operate only on classes and methods in this environment.

There are different subclasses of RBBrowserEnvironment for the different kind of 'scopes'. 

- RBClassEnvironment - only show classes/methods from a set of classes.
- RBPackageEnvironment - only show classes / packages / methods from a set of packages.
- RBSelectorEnvironment - only show classes / methods from a set of selector names.
Check next chapter for the full list of environments.

Instead of directly using the different subclasses for a scoped view, the base class `RBBrowserEnvironment` can act as a factory for creating restricted environments. See the methods in its 'environments'-protocol, on how to create the different environments.

You start with a default environment containing all classes from the system and create a new scoped environment by calling the appropriate method.

For example, creating an environment for all classes in package 'Kernel':

```st
RBBrowserEnvironment new forPackageNames:{'Kernel'}.
```

You can query the environment.

```st
| env |
env := RBBrowserEnvironment new forPackageNames:{'Kernel'}.
env allClasses 
-> a list of all classes in package Kernel
```

or open a browser

```st
env browse 
-> starts Calypso showing only this package
```

and you can further restrict this package environment by calling one of the other factory methods:

```st
env class 
-> a RBPackageEnvironment
```

```st
(env implementorsOf:#collect:) class
->  RBSelectorEnvironment
```

Another way to combine or further restrict environments is to use boolean operations and, not or or.

```st
| implDrawOn callsDrawOn implAndCalls |
callsDrawOn := RBBrowserEnvironment new referencesTo: #drawOn:.
implDrawOn :=  RBBrowserEnvironment new implementorsOf: #drawOn:.
"create an 'anded'-environment"
implAndCalls := callsDrawOn & implDrawOn.
"collect all message and open a MessageBrowser"
MessageBrowser browse: implAndCalls methods.
```

This opens a MessageBrowser on all methods in the system that implement `#drawOn:` and calls `drawOn:`.

```st
| implPrintOn notImplPrintOn |
implPrintOn := RBBrowserEnvironment new implementorsOf: #printOn:.
"create a 'not'-environment"
notImplPrintOn := implPrintOn not.
implPrintOn includesClass: Object. 
-> true
notImplPrintOn includesClass: Object. 
-> false
```

Classes implementing `#printOn:` are not in the 'not'-environment.

A more generic way to create an environment by giving an explicit 'test'-block to select methods for this environment:

```st
| implementedByMe |
implementedByMe := RBBrowserEnvironment new 
			selectMethods: [:m | m selector size > 10 ].
implementedByMe browse.
```

This opens (may be slow) a browser with all classes with methods having a selector larger than 10 characters.
