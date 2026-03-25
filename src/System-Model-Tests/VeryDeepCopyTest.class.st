"
This is the unit test for the class UndefinedObject. Unit tests are a good way to exercise the functionality of your system in a repeatable and automatic manner. They are therefore recommended if you plan to release anything. For more information, see: 
	- http://www.c2.com/cgi/wiki?UnitTest
	- there is a chapter in the PharoByExample book (http://pharobyexample.org)
	- the sunit class category
"
Class {
	#name : 'VeryDeepCopyTest',
	#superclass : 'TestCase',
	#category : 'System-Model-Tests-Core',
	#package : 'System-Model-Tests',
	#tag : 'Core'
}

{ #category : 'tests - copying' }
VeryDeepCopyTest >> testVeryDeepCopyWith [
	self assert: (nil veryDeepCopyWith: nil) isNil
]
