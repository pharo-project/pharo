"
I represent a future features for Math environment. 

I will have to be defined by an annotation, parsed and converted to my structure. 
I will have to be tested. 

Currently I'm just used by Microdown (delimited by `$$`) to make sure that we convert well the datastructures.

"
Class {
	#name : #PRMathEnvironment,
	#superclass : #PREnvironment,
	#category : #'Pillar-Core-Model'
}

{ #category : #'accessing - parameters' }
PRMathEnvironment >> accept: aVisitor [
	^ aVisitor visitMathEnvironment: self
]

{ #category : #'accessing - parameters' }
PRMathEnvironment >> text [ 
	^ self children first text
]
