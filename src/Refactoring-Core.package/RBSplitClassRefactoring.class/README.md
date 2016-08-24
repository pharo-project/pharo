I am a refactoring for extracting a set of instance variables to a new class.

You can choose which instance variables should be moved into the new class. The new class becomes an instvar of the original class and every reference to the moved variables is replaced by a accessor call.

My precondition verifies that 
 the new instance variable is a valid variable name and not yet used in this class or its hierarchy
 the name of the new class representing the set of instance variables is a valid class name

Example:
In the following class:
Object subclass: #TextKlass
	instanceVariableNames: 'text color font style'
	classVariableNames: ''
	package: 'TestKlasses'
	
the avariables color/font/style should be moved to a new "TextAttributes"-Class.
We apply the Split Refactoring with this three variables and select a new class name TextAttributes used as variable new "textAttributes".
The class definition will be changed to
Object subclass: #TextKlass
	instanceVariableNames: 'text textAttributes'
	classVariableNames: ''
	package: 'TestKlasses'
	
and every reference to the old vars 
color/font/style 
will be replaced by
 textAttributes color / textAttributes style / textAttributesFont
