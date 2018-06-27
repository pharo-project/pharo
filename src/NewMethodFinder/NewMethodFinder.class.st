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
			Send new to: receiver selector: method withArgs: args 
		]). 
	].
]

{ #category : #'as yet unclassified' }
NewMethodFinder >> filterMethods [
	"comment stating purpose of message"

   "TODO : If the class doesn't understand approvedMethods message. => Corresponding test"
	|class currents forbiddens|
	methods := OrderedCollection new.
	forbiddens := Set new.
	class := sender class.
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
NewMethodFinder >> filterMethodsWithSender: sender_ [
	"comment stating purpose of message"

   "TODO : If the class doesn't understand approvedMethods message. => Corresponding test"
	|class currents forbiddens|
	methods := OrderedCollection new.
	forbiddens := Set new.
	class := sender_ class.
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
]

{ #category : #'as yet unclassified' }
NewMethodFinder >> findMethods [
	"Select corresponding methods"

	^ sends select: [ :send | 
		send resultIn: output.
	] thenCollect: #selector. 
]

{ #category : #'as yet unclassified' }
NewMethodFinder >> guess: in and: out [
	"Search for methods which giving each elements of input returns the corresponding 
	 elements of outputs."
	
	input := in.
	output := out.
	self manageArguments.
	self filterMethodsWithSender: sender.
	sends := methods collect: [ :method | 
		Send withSender: sender selector: method andArgs: args. 
	].
	^self findMethods.

]

{ #category : #'as yet unclassified' }
NewMethodFinder >> guessOne: in and: out [
	"Search for methods which giving input returns output."
	|res|
	
	inputs := in.
	outputs := out.
	 
	(inputs isKindOf: Collection)
	ifFalse: [
			argNumber := 0.
			inputs := { inputs }
		] 
		ifTrue: [ 
			argNumber := inputs size - 1.
		].
	
	
		methods := inputs asSet inject: (inputs first first class approvedMethods) into: [ :acc :each |
		acc intersection: each first class approvedMethods
	].
	methods := methods intersection: (inputs first first class selectorsWithArgs: argNumber).
	
	
	sends := inputs collect: [ :input | Send withSender: input first andArgs: input allButFirst]. 
		
	"Creates dictionnary inputs -> outputs."
	sends doWithIndex: [ :send :index | 
		answers at: (send sender) put: (outputs at: index).	
	].

	
	res := OrderedCollection new.
	res := methods select: [ :method | 
		sends allSatisfy: [ :send | ( send sender perform: method withArguments: send arguments) = (answers at: send sender ) ]
	].
	^res.

]

{ #category : #'as yet unclassified' }
NewMethodFinder >> manageArguments [
	
	(input isKindOf: Collection) 
		ifFalse: [
			args := {}.
			sender := input.
			argNumber := 0. 
		] 
		ifTrue: [
			args := input allButFirst.
			sender := input first.
			argNumber := input size - 1.
		].
]

{ #category : #'as yet unclassified' }
NewMethodFinder >> oldFilterMethods [
	"comment stating purpose of message"

   "TODO : If the class doesn't understand approvedMethods message. => Corresponding test"
	methods := inputs asSet inject: (inputs first first class approvedMethods) into: [ :acc :each |
		acc intersection: each first class approvedMethods
	].
	methods := methods intersection: (inputs first first class selectorsWithArgs: argNumber).
	
]

{ #category : #'as yet unclassified' }
NewMethodFinder >> oldFindMethods [
	"Select corresponding methods"

	| res |
	res := OrderedCollection new.
	res := methods select: [ :method | 
		sends allSatisfy: [ :send | ( send sender perform: method withArguments: send arguments) = (answers at: send sender ) ]
	].
	^res. 
]

{ #category : #'as yet unclassified' }
NewMethodFinder >> oldManageArguments [
	
	inputs isEmpty ifTrue: [ ^self error: 'no inputs to check.'].
	inputs size = outputs size ifFalse: [^self error: 'should have as many input as output.'].
	argNumber := (inputs first isKindOf: Collection) 
		ifFalse: [
			inputs := inputs collect: [ :each | { each} ].
			0 
		] 
		ifTrue: [ 
			(inputs allSatisfy: [ :inputV | inputV size = inputs first size])
			ifTrue: [ inputs first size - 1]
			ifFalse: [ ^self error: 'each inputs should have the same size.'] 
		].
]

{ #category : #'as yet unclassified' }
NewMethodFinder >> permutationGuess: in and: out [
	"Search for methods which giving each elements of input returns the corresponding 
	 elements of outputs."
	
	input := in deepCopy.
	output := out.
	^ self createSends; findMethods.

]
