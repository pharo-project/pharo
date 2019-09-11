Class {
	#name : #HiExamplesTest,
	#superclass : #TestCase,
	#category : #'Hiedra-Tests-Model'
}

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
	exampleMethods := self exampleMethods.
	self deny: exampleMethods isEmpty.
	exampleMethods do: [ :each | 
		self
			shouldnt: [ each methodClass instanceSide perform: each selector ]
			raise: Error ]
]
