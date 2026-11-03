Class {
	#name : 'SycAbstractCommandTest',
	#superclass : 'TestCase',
	#category : 'SystemCommands-ClassCommands-Tests-Tests',
	#package : 'SystemCommands-ClassCommands-Tests',
	#tag : 'Tests'
}

{ #category : 'testing' }
SycAbstractCommandTest class >> isAbstract [

	^ self = SycAbstractCommandTest
]

{ #category : 'helpers' }
SycAbstractCommandTest >> contextOnPoint [

	^ SycMockClassContext new
		  selectedObjects: { Point };
		  yourself
]
