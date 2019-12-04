"
Instances of WeakMessageSend encapsulate messages send to objects, like MessageSend. Unlike MessageSend it is not necessarily a valid message. A request to value only results in a send if infact it is valid. 

See also MessageSend comments. WeakMessageSend is used primarily for event registration. 

Unlike MessageSend, WeakMessageSend store receivers (object receiving the message send) as the first and only element of its array as opposed to a named ivar.
But like MessageSend, it does have
 selector		Symbol -- message selector
 arguments		Array -- bound arguments
and it also has
 shouldBeNil		Boolean --  used to ensure array of arguments is not all nils
"
Class {
	#name : #WeakMessageSend,
	#superclass : #Object,
	#type : #weak,
	#instVars : [
		'selector',
		'shouldBeNil',
		'arguments'
	],
	#category : #'Kernel-Messaging'
}

{ #category : #'instance creation' }
WeakMessageSend class >> new [
	^self new: 1

]

{ #category : #'instance creation' }
WeakMessageSend class >> receiver: anObject selector: aSymbol [
	^ self receiver: anObject selector: aSymbol arguments: #()

]

{ #category : #'instance creation' }
WeakMessageSend class >> receiver: anObject selector: aSymbol argument: aParameter [
	^ self receiver: anObject selector: aSymbol arguments: (Array with: aParameter)

]

{ #category : #'instance creation' }
WeakMessageSend class >> receiver: anObject selector: aSymbol arguments: anArray [
	^ self new
		receiver: anObject;
		selector: aSymbol;
		arguments: anArray

]

{ #category : #comparing }
WeakMessageSend >> = anObject [
	"Compare equal to equivalent MessageSend"
	^ anObject isMessageSend
		and: [self receiver == anObject receiver
		and: [selector == anObject selector
		and: [(Array withAll: arguments) = (Array withAll: anObject arguments)]]]

]

{ #category : #accessing }
WeakMessageSend >> arguments [
	^arguments ifNil: [ Array new ]

]

{ #category : #accessing }
WeakMessageSend >> arguments: anArray [
	arguments := WeakArray withAll: anArray.
	"no reason this should be a WeakArray"
	shouldBeNil := Array withAll: (anArray collect: [ :ea | ea isNil ]).

]

{ #category : #converting }
WeakMessageSend >> asMessageSend [
	^MessageSend receiver: self receiver selector: selector arguments: (Array withAll: self arguments) 

]

{ #category : #converting }
WeakMessageSend >> asMinimalRepresentation [

	self isReceiverOrAnyArgumentGarbage
		ifTrue: [^nil]
		ifFalse:[^self].
]

{ #category : #private }
WeakMessageSend >> collectArguments: anArgArray [
	"Private"
    | staticArgs |
    staticArgs := self arguments.
    ^(anArgArray size = staticArgs size)
        ifTrue: [ Array withAll: anArgArray ]
        ifFalse:
            [(staticArgs isEmpty
                ifTrue: [ staticArgs := Array new: selector numArgs]
                ifFalse: [ Array withAll: staticArgs ] )
                    replaceFrom: 1
                    to: (anArgArray size min: staticArgs size)
                    with: anArgArray
                    startingAt: 1]

]

{ #category : #evaluating }
WeakMessageSend >> cull: arg [
	^ selector numArgs = 0 
		ifTrue: [ self value ]
		ifFalse: [ self value: arg ].
]

{ #category : #evaluating }
WeakMessageSend >> cull: arg1 cull: arg2 [
	^ selector numArgs < 2 
		ifTrue: [ self cull: arg1]
		ifFalse: [ self value: arg1 value: arg2 ]
]

{ #category : #evaluating }
WeakMessageSend >> cull: arg1 cull: arg2 cull: arg3 [
	^ selector numArgs < 3 
		ifTrue: [ self cull: arg1 cull: arg2 ]
		ifFalse: [ self value: arg1 value: arg2 value: arg3 ]
]

{ #category : #private }
WeakMessageSend >> ensureArguments [
	"Return true if my arguments haven't gone away"
	arguments ifNotNil: [
		arguments with: shouldBeNil do: [ :arg :flag |
			arg ifNil: [ flag ifFalse: [ ^false ]]
		]
	].
	^true

]

{ #category : #private }
WeakMessageSend >> ensureReceiver [
  "Return true if my receiver hasn't gone away"
  self receiver ifNil: [^ false].
  (self receiver class isObsolete) ifTrue: [^ false].
  (self receiver isBehavior and: [self receiver isObsolete]) ifTrue: [^ false].
  ^ true.

]

{ #category : #private }
WeakMessageSend >> ensureReceiver: anObject [
  "Return true if my receiver hasn't gone away"
  anObject ifNil: [^ false].
  (anObject class isObsolete) ifTrue: [^ false].
  (anObject isBehavior and: [anObject isObsolete]) ifTrue: [^ false].
  ^ true.

]

{ #category : #private }
WeakMessageSend >> ensureReceiverAndArguments [

  "Return true if my receiver hasn't gone away"
  self receiver ifNil: [^ false].
  (self receiver class isObsolete) ifTrue: [^ false].
  (self receiver isBehavior and: [self receiver isObsolete]) ifTrue: [^ false].

  "Make sure that my arguments haven't gone away"
  arguments ifNotNil: [
    arguments with: shouldBeNil do: [ :arg :flag |
      arg ifNil: [ flag ifFalse: [ ^false ]]
    ]
  ].

  ^true

]

{ #category : #private }
WeakMessageSend >> ensureReceiverAndArguments: aReceiver [

  "Return true if my receiver hasn't gone away"
  aReceiver ifNil: [^ false].
  (aReceiver class isObsolete) ifTrue: [^ false].
  (aReceiver isBehavior and: [self receiver isObsolete]) ifTrue: [^ false].

  "Make sure that my arguments haven't gone away"
  arguments ifNotNil: [
    arguments with: shouldBeNil do: [ :arg :flag |
      arg ifNil: [ flag ifFalse: [ ^false ]]
    ]
  ].

  ^true

]

{ #category : #comparing }
WeakMessageSend >> hash [
	"work like MessageSend>>hash"
	^self receiver hash bitXor: selector hash

]

{ #category : #private }
WeakMessageSend >> isAnyArgumentGarbage [
	"Make sure that my arguments haven't gone away"
	arguments ifNotNil: [
		arguments with: shouldBeNil do: [ :arg :flag |
			(flag not and: [arg isNil])
				ifTrue: [^true]
		]
	].
	^false

]

{ #category : #testing }
WeakMessageSend >> isMessageSend [
	^true

]

{ #category : #private }
WeakMessageSend >> isReceiverGarbage [
	"Make sure that my receiver hasn't gone away"
	^self receiver isNil

]

{ #category : #private }
WeakMessageSend >> isReceiverOrAnyArgumentGarbage [
	"Make sure that my receiver hasn't gone away"
	^self isReceiverGarbage 
		or: [self isAnyArgumentGarbage]

]

{ #category : #testing }
WeakMessageSend >> isValid [
	^self isReceiverOrAnyArgumentGarbage not

]

{ #category : #printing }
WeakMessageSend >> printOn: aStream [

	super printOn: aStream.
	
	aStream 
		nextPut: $(;
		print: selector;
		nextPutAll: ' -> ';
		print: self receiver;
		nextPut: $).

]

{ #category : #accessing }
WeakMessageSend >> receiver [
	^self at: 1

]

{ #category : #accessing }
WeakMessageSend >> receiver: anObject [
	self at: 1 put: anObject

]

{ #category : #accessing }
WeakMessageSend >> selector [
	^selector

]

{ #category : #accessing }
WeakMessageSend >> selector: aSymbol [
	selector := aSymbol

]

{ #category : #evaluating }
WeakMessageSend >> value [

	| strongReceiver |

	strongReceiver := self receiver.

	^ arguments
		ifNil: [ ( self ensureReceiver: strongReceiver )
				ifTrue: [ strongReceiver perform: selector ]
				ifFalse: [  ]
			]
		ifNotNil: [ ( self ensureReceiverAndArguments: strongReceiver )
				ifTrue: [ strongReceiver perform: selector withArguments: ( Array withAll: arguments ) ]
				ifFalse: [  ]
			]
]

{ #category : #evaluating }
WeakMessageSend >> value: anObject [

	| strongReceiver |
	
	strongReceiver := self receiver.

	(self ensureReceiver: strongReceiver) ifFalse: [ ^ nil ].
	
	^ strongReceiver
		perform: selector 
		with: anObject
]

{ #category : #evaluating }
WeakMessageSend >> value: anObject1 value: anObject2 [ 

	| strongReceiver |
	
	strongReceiver := self receiver.

	(self ensureReceiver: strongReceiver) ifFalse: [ ^ nil ].
	
	^ strongReceiver 
		perform: selector 
		with: anObject1
		with: anObject2
]

{ #category : #evaluating }
WeakMessageSend >> value: anObject1 value: anObject2 value: anObject3 [

	| strongReceiver |
	
	strongReceiver := self receiver.

	(self ensureReceiver: strongReceiver) ifFalse: [ ^ nil ].
	
	^ strongReceiver
		perform: selector 
		with: anObject1
		with: anObject2
		with: anObject3
]

{ #category : #evaluating }
WeakMessageSend >> valueWithArguments: anArray [

	| strongReceiver |
	
	strongReceiver := self receiver.

	(self ensureReceiverAndArguments: strongReceiver) ifFalse: [ ^nil ].
	^ strongReceiver 
		perform: selector 
		withArguments: (self collectArguments: anArray)
]

{ #category : #evaluating }
WeakMessageSend >> valueWithEnoughArguments: anArray [
	"call the selector with enough arguments from arguments and anArray"
	| args strongReceiver |
	
	strongReceiver := self receiver.
	
	(self ensureReceiverAndArguments: strongReceiver) ifFalse: [ ^nil ].
	args := Array new: selector numArgs.
	args replaceFrom: 1
		to: ( arguments size min: args size)
		with: arguments
		startingAt: 1.
	args size > arguments size ifTrue: [
		args replaceFrom: arguments size + 1
			to: (arguments size + anArray size min: args size)
			with: anArray
			startingAt: 1.
	].
	^ strongReceiver perform: selector withArguments: args

]
