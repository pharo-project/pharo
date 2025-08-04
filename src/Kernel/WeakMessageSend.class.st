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
	#name : 'WeakMessageSend',
	#superclass : 'MessageSend',
	#type : 'weak',
	#instVars : [
		'shouldBeNil'
	],
	#category : 'Kernel-Messaging',
	#package : 'Kernel',
	#tag : 'Messaging'
}

{ #category : 'instance creation' }
WeakMessageSend class >> new [
	^self new: 1
]

{ #category : 'comparing' }
WeakMessageSend >> = anObject [
	"Compare equal to equivalent MessageSend"
	^ anObject isMessageSend
		and: [self receiver == anObject receiver
		and: [selector == anObject selector
		and: [(Array withAll: arguments) = (Array withAll: anObject arguments)]]]
]

{ #category : 'accessing' }
WeakMessageSend >> arguments [
	<reflection: 'Message sending and code execution - Message send reification'>
	^arguments ifNil: [ Array new ]
]

{ #category : 'accessing' }
WeakMessageSend >> arguments: anArray [
	<reflection: 'Message sending and code execution - Message send reification'>
	arguments := WeakArray withAll: anArray.
	"no reason this should be a WeakArray"
	shouldBeNil := Array withAll: (anArray collect: [ :ea | ea isNil ])
]

{ #category : 'converting' }
WeakMessageSend >> asMessageSend [
	^MessageSend receiver: self receiver selector: selector arguments: (Array withAll: self arguments)
]

{ #category : 'converting' }
WeakMessageSend >> asMinimalRepresentation [

	^ self isReceiverOrAnyArgumentGarbage
		  ifTrue: [ nil ]
		  ifFalse: [ self ]
]

{ #category : 'private' }
WeakMessageSend >> collectArguments: anArgArray [
	"Private"
	<reflection: 'Message sending and code execution - Message send reification'>
	| staticArgs |
	staticArgs := self arguments.
	^ anArgArray size = staticArgs size
		  ifTrue: [ Array withAll: anArgArray ]
		  ifFalse: [
			  (staticArgs ifEmpty: [ staticArgs := Array new: selector numArgs ] ifNotEmpty: [ Array withAll: staticArgs ])
				  replaceFrom: 1
				  to: (anArgArray size min: staticArgs size)
				  with: anArgArray
				  startingAt: 1 ]
]

{ #category : 'private' }
WeakMessageSend >> ensureArguments [
	"Return true if my arguments haven't gone away"
	arguments ifNotNil: [
		arguments with: shouldBeNil do: [ :arg :flag |
			arg ifNil: [ flag ifFalse: [ ^false ]]
		]
	].
	^true
]

{ #category : 'private' }
WeakMessageSend >> ensureReceiver [
  "Return true if my receiver hasn't gone away"
  self receiver ifNil: [^ false].
  (self receiver class isObsolete) ifTrue: [^ false].
  (self receiver isBehavior and: [self receiver isObsolete]) ifTrue: [^ false].
  ^ true
]

{ #category : 'private' }
WeakMessageSend >> ensureReceiver: anObject [
  "Return true if my receiver hasn't gone away"
  anObject ifNil: [^ false].
  (anObject class isObsolete) ifTrue: [^ false].
  (anObject isBehavior and: [anObject isObsolete]) ifTrue: [^ false].
  ^ true
]

{ #category : 'private' }
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

{ #category : 'private' }
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

{ #category : 'private' }
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

{ #category : 'private' }
WeakMessageSend >> isReceiverGarbage [
	"Make sure that my receiver hasn't gone away"
	^self receiver isNil
]

{ #category : 'private' }
WeakMessageSend >> isReceiverOrAnyArgumentGarbage [
	"Make sure that my receiver hasn't gone away"
	^self isReceiverGarbage
		or: [self isAnyArgumentGarbage]
]

{ #category : 'testing' }
WeakMessageSend >> isValid [
	<reflection: 'Message sending and code execution - Message send reification'>
	^self isReceiverOrAnyArgumentGarbage not
]

{ #category : 'printing' }
WeakMessageSend >> printOn: aStream [

	super printOn: aStream.

	aStream
		nextPut: $(;
		print: selector;
		nextPutAll: ' -> ';
		print: self receiver;
		nextPut: $)
]

{ #category : 'accessing' }
WeakMessageSend >> receiver [
	<reflection: 'Message sending and code execution - Message send reification'>
	^self at: 1
]

{ #category : 'accessing' }
WeakMessageSend >> receiver: anObject [
	<reflection: 'Message sending and code execution - Message send reification'>
	self at: 1 put: anObject
]

{ #category : 'evaluating' }
WeakMessageSend >> value [
	<reflection: 'Message sending and code execution - Runtime and Evaluation'>

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

{ #category : 'evaluating' }
WeakMessageSend >> value: anObject [
	<reflection: 'Message sending and code execution - Runtime and Evaluation'>
	| strongReceiver |

	strongReceiver := self receiver.

	(self ensureReceiver: strongReceiver) ifFalse: [ ^ nil ].

	^ strongReceiver
		perform: selector
		with: anObject
]

{ #category : 'evaluating' }
WeakMessageSend >> value: anObject1 value: anObject2 [
	<reflection: 'Message sending and code execution - Runtime and Evaluation'>
	| strongReceiver |

	strongReceiver := self receiver.

	(self ensureReceiver: strongReceiver) ifFalse: [ ^ nil ].

	^ strongReceiver
		perform: selector
		with: anObject1
		with: anObject2
]

{ #category : 'evaluating' }
WeakMessageSend >> value: anObject1 value: anObject2 value: anObject3 [
	<reflection: 'Message sending and code execution - Runtime and Evaluation'>
	| strongReceiver |

	strongReceiver := self receiver.

	(self ensureReceiver: strongReceiver) ifFalse: [ ^ nil ].

	^ strongReceiver
		perform: selector
		with: anObject1
		with: anObject2
		with: anObject3
]

{ #category : 'evaluating' }
WeakMessageSend >> valueWithArguments: anArray [
	<reflection: 'Message sending and code execution - Runtime and Evaluation'>
	| strongReceiver |

	strongReceiver := self receiver.

	(self ensureReceiverAndArguments: strongReceiver) ifFalse: [ ^nil ].
	^ strongReceiver
		perform: selector
		withArguments: (self collectArguments: anArray)
]

{ #category : 'evaluating' }
WeakMessageSend >> valueWithEnoughArguments: anArray [
	"call the selector with enough arguments from arguments and anArray"
	<reflection: 'Message sending and code execution - Runtime and Evaluation'>
	| newArgs strongReceiver |

	strongReceiver := self receiver.

	(self ensureReceiverAndArguments: strongReceiver) ifFalse: [ ^nil ].
	newArgs := Array new: selector numArgs.
	newArgs replaceFrom: 1
		to: ( arguments size min: newArgs size)
		with: arguments
		startingAt: 1.
	newArgs size > arguments size ifTrue: [
		newArgs replaceFrom: arguments size + 1
			to: (arguments size + anArray size min: newArgs size)
			with: anArray
			startingAt: 1.
	].
	^ strongReceiver perform: selector withArguments: newArgs
]
