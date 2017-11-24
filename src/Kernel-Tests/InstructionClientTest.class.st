"
This is the unit test for the class InstructionClient. Unit tests are a good way to exercise the functionality of your system in a repeatable and automatic manner. They are therefore recommended if you plan to release anything. For more information, see: 
	- http://www.c2.com/cgi/wiki?UnitTest
	- there is a chapter in the PharoByExample book (http://pharobyexample.org)
	- the sunit class category
"
Class {
	#name : #InstructionClientTest,
	#superclass : #TestCase,
	#category : #'Kernel-Tests-Methods'
}

{ #category : #tests }
InstructionClientTest >> testInstructions [
	"just interpret all of methods of Object"

	| client |
	client := InstructionClient new.
	Object methods
		do: [ :method | 
			| scanner |
			scanner := InstructionStream on: method.
			[ scanner pc <= method endPC ] whileTrue: [ scanner interpretNextInstructionFor: client ] ]
]
