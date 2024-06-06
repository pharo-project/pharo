"
I am a refactoring condition that checks if a method defined in `class` is called using ""super"" from a class in the `targetClass` subclasses (except for `class` and its subclasses). For clarity, `class` is one of `targetClass` subclasses.
"
Class {
	#name : 'ReMethodsReceiveNoSupersendsCondition',
	#superclass : 'ReMethodsCondition',
	#instVars : [
		'class',
		'selectors',
		'targetSuperclass'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'private' }
ReMethodsReceiveNoSupersendsCondition >> checkSiblingSuperSendsFrom: aRBClass [
	"Checks if `aRBClass` selector sends super message to one of the `selectors`
	from the `class`.
	If that stands and the super message selector is defined in `class` or one of its superclasses
	warn the user.
	Repeat the process for all subclasses of `aRBClass`"

	aRBClass selectors do: [ :each |
		| tree |
		tree := aRBClass parseTreeForSelector: each.
		tree ifNotNil: [
			tree superMessages do: [ :aSelector |
				(selectors includes: aSelector) ifTrue: [
					| definer |
					definer := aRBClass superclass whichClassIncludesSelector:
						           aSelector.

					(definer isNotNil and: [ class includesClass: definer ]) ifTrue: [
						violators add: { aSelector. aRBClass. each }  ] ] ] ] ].
	aRBClass allSubclasses do: [ :each |
		self checkSiblingSuperSendsFrom: each ]
]

{ #category : 'private' }
ReMethodsReceiveNoSupersendsCondition >> checkSuperSendsFromSiblings [
	"Checks if a class in the `targetClass` subclasses (except for `class` and its subclasses)
	send super message to one of the selected messages."

	| siblings |
	targetSuperclass name == #Object ifTrue: [ ^ self ].
	siblings := targetSuperclass subclasses reject: [:each | each = class].
	siblings do: [:aRBClass | self checkSiblingSuperSendsFrom: aRBClass]
]

{ #category : 'initialization' }
ReMethodsReceiveNoSupersendsCondition >> class: aRBClass targetSuperclass: aRBClass2 selectors: aCollection [ 
	class := aRBClass.
	targetSuperclass := aRBClass2.
	selectors := aCollection
]

{ #category : 'displaying' }
ReMethodsReceiveNoSupersendsCondition >> violationMessageOn: aStream [
	| messageSent senderClass senderMessage |
	
	self violators do: [ :violator |
		messageSent := violator at: 1.
		senderClass := violator at: 2.
		senderMessage := violator at: 3.
		
		aStream
			nextPutAll: senderClass name;
			nextPutAll: '>>';
			nextPutAll: senderMessage;
			nextPutAll: 'sends a supermessage to ';
			nextPutAll: messageSent;
			space ]
]

{ #category : 'accessing' }
ReMethodsReceiveNoSupersendsCondition >> violators [
	violators ifNotNil: [ ^ violators ].
	violators := #() asOrderedCollection.
	self checkSuperSendsFromSiblings.
	^ violators
]
