Class {
	#name : #HiExamplesTest,
	#superclass : #TestCase,
	#category : #'Hiedra-Tests-Model'
}

{ #category : #testing }
HiExamplesTest class >> isAbstract [
	^ self = HiExamplesTest
]

{ #category : #tests }
HiExamplesTest >> exampleClassToTest [
	^ self subclassResponsibility
]

{ #category : #tests }
HiExamplesTest >> exampleMethods [

	^ self exampleClassToTest classSide methods select: [ :each | 
		  (each selector beginsWith: 'example') and: [ each numArgs = 0 ] ]
]

{ #category : #tests }
HiExamplesTest >> testAllExamples [
	| exampleMethods |
	
	self timeLimit: 1 minute.
	
	exampleMethods := self exampleMethods.
	self deny: exampleMethods isEmpty.
	
	exampleMethods do: [ :each | | anExample |
		self
			shouldnt: [ anExample := each methodClass instanceSide perform: each selector ]
			raise: Error.
		anExample ifNotNil: [ anExample close ] ]
]
