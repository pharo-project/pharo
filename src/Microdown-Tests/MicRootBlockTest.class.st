"
I'm a test class for testing the behavior of MicRootBlock
"
Class {
	#name : #MicRootBlockTest,
	#superclass : #MicBlockTest,
	#category : #'Microdown-Tests-Parser'
}

{ #category : #tests }
MicRootBlockTest >> testRootCanConsumeLine [
	"a root block can consume anything"
	
	| rootNode | 
	rootNode := MicRootBlock new.
	self assert: (rootNode canConsumeLine: '#').
	self assert: (rootNode canConsumeLine: '- ')
]

{ #category : #tests }
MicRootBlockTest >> testedClass [ 
	^ MicRootBlock 
]
