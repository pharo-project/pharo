Class {
	#name : 'RubFindReplaceServiceTest',
	#superclass : 'TestCase',
	#category : 'Rubric-Tests-Base',
	#package : 'Rubric-Tests',
	#tag : 'Base'
}

{ #category : 'tests' }
RubFindReplaceServiceTest >> defaultFindReplaceServiceClass [

	^ RubFindReplaceService
]

{ #category : 'tests' }
RubFindReplaceServiceTest >> testCaseSensitive [

	self deny: ( self defaultFindReplaceServiceClass newFor: self) caseSensitive
]
