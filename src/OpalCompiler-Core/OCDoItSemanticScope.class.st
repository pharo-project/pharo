"
I am a root of hierarchy of scopes representing the various kindos of DoIt expressions
"
Class {
	#name : #OCDoItSemanticScope,
	#superclass : #OCSemanticScope,
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #testing }
OCDoItSemanticScope class >> isAbstract [ 
	^self = OCDoItSemanticScope 
]

{ #category : #converting }
OCDoItSemanticScope >> asDoItScope [
	^self
]

{ #category : #testing }
OCDoItSemanticScope >> isDoItScope [ 
	^true
]
