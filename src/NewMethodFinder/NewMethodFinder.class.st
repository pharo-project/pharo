"
I'm a method finder that will find methods from given examples in an array.
The examples array should contain one object for the receiver, one object per expected argument and then a final object with the expected result.
In other words 
 - a unary method example expects an array of input objects #( receiver ) and an expected result
 - a binary method example expects an array with two input objects #( receiver argument ) and an expected result
 - a keyword method example expects an array with at least two elements  #( receiver argument1 argument2 ... ) and an expected results

The method finder will take the input objects (receiver and arguments) and perform their permutation to be able to find more results.
Then, it will lookup in the receiver's hierarchy the approuved and forbidden methods to run on the hierarchy and run them on the permutation of objects.

NewMethodFinder new findMethodsByExampleInput: #( 1 2 ) andExpectedResult: 3
"
Class {
	#name : #NewMethodFinder,
	#superclass : #Object,
	#category : #NewMethodFinder
}

{ #category : #'as yet unclassified' }
NewMethodFinder >> findMethodsByExampleInput: inputCollection andExpectedResult: expectedResult [
	"Search for methods which giving each elements of input returns the corresponding 
	 elements of outputs."

	^ (self possibleSolutionsForInput: inputCollection) select: [ :send | send resultIn: expectedResult ].
]

{ #category : #testing }
NewMethodFinder >> isSafe: selector [
	Transcript show: selector implementors first class; cr.
	^ selector implementors allSatisfy: [ :each | (self isSafe: selector in: each)].
]

{ #category : #testing }
NewMethodFinder >> isSafe: selector in: class [
	|actual|
	
	actual := class.
	[actual isNil] whileFalse: [
		(actual class selectors includes: #forbiddenSelectors) ifTrue: [
			(actual forbiddenSelectors includes: selector) ifTrue: [ ^ false ]. 
		].
		(actual class selectors includes: #approvedSelectors) ifTrue: [
			Transcript show: actual approvedSelectors; cr.
			(actual approvedSelectors includes: selector) ifTrue: [ ^ true ].  
		].
		actual := actual superclass.
	].
	^ false.
]

{ #category : #'as yet unclassified' }
NewMethodFinder >> possibleSolutionsForInput: inputCollection [

	| sends |
	sends := OrderedCollection new.
	inputCollection permutationsDo: [ :permutation | | foundPermutationSends args receiver |
		args := permutation allButFirst deepCopy.
		receiver := permutation first deepCopy.
		foundPermutationSends := ((receiver class allSelectorsToTestWithArity: (inputCollection size - 1))
			collect: [ :method | NewMethodFinderSend new to: receiver selector: method withArgs: args ]).
		sends addAll: foundPermutationSends ].
	^  sends
]
