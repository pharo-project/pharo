Class {
	#name : 'VirtualManagerTest',
	#superclass : 'TestCase',
	#category : 'System-Support-Tests-Image',
	#package : 'System-Support-Tests',
	#tag : 'Image'
}

{ #category : 'tests' }
VirtualManagerTest >> testVersionIsInVersionString [

	| versionString versionNumber |

	versionString := Smalltalk vm version.
	versionNumber := $. join: (Smalltalk vm vmVersion collect: [ :e | e printString ]).
	
	self assert: (versionString includesSubstring: versionNumber) 
]
