RGElementDefinition is the abstract class for representing elements of a class-alike definition (i.e., methods, variables, comment).

parent holds the RGClassDefinition or RGMetaclassDefinition defining this element.
	
	
Now a RingEntityDefinition offers two APIs: one that is generic and works for all the source code entities and this is the one we just 
presented: parent, parentName and realParent. Having such interface is important to build generic tools that could manipulate 
any entities in a polymorphic way (yes no isKindOf: everywhere).

In addition, a ring method definition offers a specific interface that should only be used when you know that you are solely manipulate
specific entity such as class element: method definition, class comment, and variables. 

Here is the equivalence table

	realParent 				realClass
	parent					ringClass
	parentName			className
	
For example for a methodDefinition we will have the following:

GENERIC API
------------------
* To access the ring class definition name, use parentName
	aRGMethodDefinition parentName
	
Example:
	(Point>>#dist:) asRingDefinition parentName
		->  #Point
		
* If you have a complete model where classes and methods are ring definition, to access the ring class definition , use parent
	aRGMethodDefinition parent
	
Example:
	aRGMethodDefinition(Point>>#dist:) parent
		->  aRGClassDefinition(Point)
		
* If you want to access the smalltalk class that contains the compiledMethod that is represented by a ringMethodDefinition, use realParent
	aRGMethodDefinition realParent
	
Example:
	(Point>>#dist:) asRingDefinition realParent
		->  Point
		


CLASS Element specific API
------------------------------------------
* The message class returns the class of the object :). Yes as you see we could not use class and className because class is already used to refer to the class of the object.

Example:
	(Point>>#dist:) asRingDefinition class
		->  RingMethodDefinition
		
* The message className returns the name of the ring class defining the reingMethodDefinition.

Example:
	(Point>>#dist:) asRingDefinition className
		->  #Point		
		
* If you have a complete model where classes and methods are ring definition, to access the ring class definition , use parent
	aRGMethodDefinition ringClass
	
Example:
	aRGMethodDefinition(Point>>#dist:) ringClass
		->  aRGClassDefinition(Point)
		
		
* If you want to access the smalltalk class that contains the compiledMethod that is represented by a ringMethodDefinition, use realClass
	aRGMethodDefinition realClass
	
Example:
	(Point>>#dist:) asRingDefinition realClass
		->  Point

