Class {
	#name : #RubFindReplaceServiceTest,
	#superclass : #TestCase,
	#category : #'Rubric-Tests'
}

{ #category : #tests }
RubFindReplaceServiceTest >> defaultFindReplaceServiceClass [

	^ RubFindReplaceService
]

{ #category : #tests }
RubFindReplaceServiceTest >> testCaseSensitive [ 
	
	self deny: ( self defaultFindReplaceServiceClass newFor: self) caseSensitive
]
