Class {
	#name : #HiExamplesTest,
	#superclass : #TestCase,
	#category : #'Hiedra-Tests-Model'
}

{ #category : #accessing }
HiExamplesTest class >> defaultTimeLimit [ 

	^ 30 seconds
]

{ #category : #tests }
HiExamplesTest >> exampleMethods [ 
	^ (((RPackage organizer packages
		select: [ :each | each name beginsWith: 'Hiedra' ])
		flatCollect: #classes)
		flatCollect: [ :each | each classSide methods ])
		select: [ :each | 
			(each selector beginsWith: 'example') and: [ each numArgs = 0 ] ]
]

{ #category : #tests }
HiExamplesTest >> testAllExamples [
	| exampleMethods |
	
	self skip: 'This test fails randomly in the CI in different OS due a timeout, even with an increased time limit of 30 seconds'.
	self timeLimit: 30 seconds.
	
	exampleMethods := self exampleMethods.
	self deny: exampleMethods isEmpty.
	
	exampleMethods do: [ :each | | anExample |
		self
			shouldnt: [ anExample := each methodClass instanceSide perform: each selector ]
			raise: Error.
		anExample ifNotNil: [ anExample close ] ]
]
