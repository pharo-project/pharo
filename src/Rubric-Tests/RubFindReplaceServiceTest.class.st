Class {
	#name : #RubFindReplaceServiceTest,
	#superclass : #TestCase,
	#category : #'Rubric-Tests'
}

{ #category : #tests }
RubFindReplaceServiceTest >> testCaseSensitive [ 
	
	self deny: RubFindReplaceService new caseSensitive
]
