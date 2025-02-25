"
An ALSourceCleanerTest is a test class for testing the behavior of ALSourceCleaner
"
Class {
	#name : 'ReMethodSourceCleanerTest',
	#superclass : 'TestCase',
	#category : 'Renraku-Tests-Utilities',
	#package : 'Renraku-Tests',
	#tag : 'Utilities'
}

{ #category : 'private - test data' }
ReMethodSourceCleanerTest >> aleadyCleanedMethodStringKeepingDot [
	"This method returns the method body of #dirtyMethodWithUnnecessarySpacesAndFinalDot with a final dot"

	^'dirtyMethodWithUnnecessarySpacesAndFinalDot
	"Do not format this method - it by intention should include unnecessary spaces and a final dot"

	|temp|

	temp := 3 + 4.'
]

{ #category : 'private - test data' }
ReMethodSourceCleanerTest >> alreadyCleanedMethodString [
	"This method returns the method body of #dirtyMethodWithUnnecessarySpacesAndFinalDot without a final dot"

	^'dirtyMethodWithUnnecessarySpacesAndFinalDot
	"Do not format this method - it by intention should include unnecessary spaces and a final dot"

	|temp|

	temp := 3 + 4'
]

{ #category : 'private - test data' }
ReMethodSourceCleanerTest >> dirtyMethodWithUnnecessarySpacesAndFinalDot [
	"Do not format this method - it by intention should include unnecessary spaces and a final dot"

	|temp|

	temp := 3 + 4
]

{ #category : 'private - test data' }
ReMethodSourceCleanerTest >> properMethodWithFinalDot [
	"Do not format this method - this is a method without any unnecessary separators in the method body but a final dot"

	^ 3 + 4.
]

{ #category : 'private - test data' }
ReMethodSourceCleanerTest >> properMethodWithoutFinalDot [
	"Do not format this method - this is a method without any unnecessary separators in the method body and without a final dot"

	^ 3 + 4
]

{ #category : 'tests - cleaning' }
ReMethodSourceCleanerTest >> testCleaning [

	| cleaned method |
	method := ReMethodSourceCleanerTest>>#dirtyMethodWithUnnecessarySpacesAndFinalDot.
	cleaned := ReMethodSourceCleaner clean: method sourceCode.

	self assert: cleaned equals: self alreadyCleanedMethodString
]

{ #category : 'tests - cleaning' }
ReMethodSourceCleanerTest >> testCleaningNotKeepingDot [

	| cleaned method |
	method := ReMethodSourceCleanerTest >> #dirtyMethodWithUnnecessarySpacesAndFinalDot.
	cleaned := ReMethodSourceCleaner clean: method sourceCode keepFinalDot: false.
	self assert: cleaned equals: self alreadyCleanedMethodString
]

{ #category : 'tests - cleaning' }
ReMethodSourceCleanerTest >> testFollowsSettingForCleaningFinalDotUponInstantiation [

	self
		assert: ReMethodSourceCleaner new keepFinalDot
		equals: ReMethodSourceCleaner keepFinalDotInMethodBody
]

{ #category : 'tests - cleaning' }
ReMethodSourceCleanerTest >> testIsNecessaryToCleanProperMethodWithFinalDot [

	| method |
	method := ReMethodSourceCleanerTest >> #properMethodWithFinalDot.

	self assert: (ReMethodSourceCleaner isNecessaryToClean: method allowFinalDot: false)
]

{ #category : 'tests - cleaning' }
ReMethodSourceCleanerTest >> testIsNotNecessaryToCleanAnAlreadyCleanMethod [

	| method |
	method := ReMethodSourceCleanerTest >> #alreadyCleanedMethodString.

	self deny:
		(ReMethodSourceCleaner
			 isNecessaryToClean: method
			 allowFinalDot: false)
]

{ #category : 'tests' }
ReMethodSourceCleanerTest >> testIsNotNecessaryToCleanProperMethodWithFinalDot [

	| method |
	method := ReMethodSourceCleanerTest >> #properMethodWithFinalDot.

	self deny: (ReMethodSourceCleaner isNecessaryToClean: method allowFinalDot: true)
]

{ #category : 'tests' }
ReMethodSourceCleanerTest >> testIsNotNecessaryToCleanProperMethodWithoutFinalDot [

	| method |
	method := ReMethodSourceCleanerTest >> #properMethodWithoutFinalDot.

	self deny: (ReMethodSourceCleaner isNecessaryToClean: method allowFinalDot: false)
]

{ #category : 'tests - settings' }
ReMethodSourceCleanerTest >> testKeepFinalDotIsDisabledByDefault [

	self assert: ReMethodSourceCleaner keepFinalDotInMethodBody equals: false
]
