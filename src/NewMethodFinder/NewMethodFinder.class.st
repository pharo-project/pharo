Class {
	#name : #NewMethodFinder,
	#superclass : #Object,
	#instVars : [
		'sends',
		'input',
		'output'
	],
	#category : #NewMethodFinder
}

{ #category : #'as yet unclassified' }
NewMethodFinder >> createSends [

	|argNumber args receiver|

	argNumber := input size - 1.
	sends := OrderedCollection new.
	input permutationsDo: [ :permutation |
		args := permutation allButFirst.
		receiver := permutation first.
		sends addAll: ((self filterMethodsWithReceiver: receiver argNumber: argNumber) collect: [ :method |
			NewMethodFinderSend new to: receiver selector: method withArgs: args 
		]). 
	].
]

{ #category : #'as yet unclassified' }
NewMethodFinder >> filterMethodsWithReceiver: receiver argNumber: argNumber [

	|class currents forbiddens methods|
	methods := OrderedCollection new.
	forbiddens := Set new.
	class := receiver class.
	[class isNil] whileFalse: [ 
		currents := class selectorsWithArgs: argNumber.
		(class class selectors includes: #forbiddenMethods) ifTrue: [
			forbiddens addAll: class forbiddenMethods.
		].
		(class class selectors includes: #approvedMethods) ifTrue: [ 
			methods addAll: ((class approvedMethods intersection: currents) copyWithoutAll: forbiddens).
		].
		class := class superclass.
	 ].
	^ methods. 
]

{ #category : #'as yet unclassified' }
NewMethodFinder >> findMethods [
	"Select corresponding methods"

	^ sends select: [ :send | 
		send resultIn: output.
	] thenCollect: #selector. 
]

{ #category : #'as yet unclassified' }
NewMethodFinder >> findSends [
	"Select corresponding methods"

	^ sends select: [ :send | 
		send resultIn: output.
	]. 
]

{ #category : #'as yet unclassified' }
NewMethodFinder >> finderGuess: in and: out [
	"Search for methods which giving each elements of input returns the corresponding 
	 elements of outputs."
	
	input := in deepCopy.
	output := out.
	^ self createSends; findSends.

]

{ #category : #'as yet unclassified' }
NewMethodFinder >> permutationGuess: in and: out [
	"Search for methods which giving each elements of input returns the corresponding 
	 elements of outputs."
	
	input := in deepCopy.
	output := out.
	^ self createSends; findMethods.

]
