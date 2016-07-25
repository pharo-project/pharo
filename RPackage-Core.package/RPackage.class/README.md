A RPackage is a new implementation of package. Note that it does not touch classes but it is just a layer on top of classes therefore removing a method from a package does not change the underlying class. 


Instance Variables
	classDefinedSelectors:		Dictionary
	classExtensionSelectors:		Dictionary
	metaclassDefinedSelectors:		Dictionary
	metaclassExtensionSelectors:		Dictionary
	packageName:		String
	classes: OrderedCollection of Symbols
	classTags: is a mapping from tags to classNames

The reason we use four dictionaries is that this way we only store #Point in both as the class and the metaclass are involved. We do not have to with Point_class problem when comparing defined and extended classes. The idea is taken from the RBSelectorEnvironment. 					
		
Class Invariant one:
	Classes should not contain metaclass name, but only class names
	
	

Implementation notes 
====================
This class went over 3 internal representations implementation. 
	- first: 	a list of class + two dictionaries: class * methods
	This was not good since we add to go all the time over the list of classes.
	- second: 4 dictionaries class * selectors 
	This was not good since we want to have defined classes without methods. 
	- third: 4 dictionaries + definedClasses

Originally I wanted to avoid to have a defined class list and I wanted to avoid to have to declare the class as defined. But this is not really good since
	- we will want to know if a class definition (without method for example is defined in a given package)
	- second this is easier to hook the packageOf: behavior (we register the package class when the class is 	added to the package).

defined classes information is redundant with the dictionary keys of defined methods but we would have to check and register the class to the packageOrganizer the first time a method is defined then also check on remove to unregister the class from the packageOrganizer.

Adding a method does not define the class as a defined package class. This has to be done explictly. The reason for this choice is that a class should register to the packageOrganizer and that I do not want to do it each time a method is added and I do not want to test it each time a method is added. Now this is done only when the class is declared as defined. 
We could also give the complete freedom to the client to register the class but I thought it was a good compromise. 
	
	
ClassTag are tags that can be associated to classes. They help user organizing their class internal. 
So that we can have a package infrastructure as follows:
	Package1
		ClassA
		ClassB
	in case there is no tags associated to the package
	or
	Package2	
		Tag1
			ClassA
			ClassB
		Tag2
			ClassC
			ClassD
			ClassE
			ClassA		
	
Todo
====
	Next: 
	- finish 
	- build up a synchronizer that import PackageInfo.
	

	

	
