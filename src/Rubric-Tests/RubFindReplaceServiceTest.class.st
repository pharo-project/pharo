Class {
	#name : #RubFindReplaceServiceTest,
	#superclass : #TestCase,
	#category : #'Rubric-Tests-Base'
}

{ #category : #tests }
RubFindReplaceServiceTest >> defaultFindReplaceServiceClass [

	^ RubFindReplaceService
]

{ #category : #tests }
RubFindReplaceServiceTest >> testCaseSensitive [ 
	
	self deny: ( self defaultFindReplaceServiceClass newFor: self) caseSensitive
]
