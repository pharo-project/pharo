"
Instances of MessageSend encapsulate messages send to objects. Arguments can be either predefined or supplied when the message send is performed. 

Use #value to perform a message send with its predefined arguments and #valueWithArguments: if additonal arguments have to supplied.

Structure:
 receiver		Object -- object receiving the message send
 selector		Symbol -- message selector
 arguments		Array -- bound arguments
"
Class {
	#name : #MessageSend,
	#superclass : #Object,
	#instVars : [
		'receiver',
		'selector',
		'arguments'
	],
	#category : #'Kernel-Messaging'
}

{ #category : #'instance creation' }
MessageSend class >> message: aMessage to: anObject [
	^ self receiver: anObject selector: aMessage selector arguments: aMessage arguments
]

{ #category : #'instance creation' }
MessageSend class >> receiver: anObject selector: aSymbol [
	^ self receiver: anObject selector: aSymbol arguments: #()
]

{ #category : #'instance creation' }
MessageSend class >> receiver: anObject selector: aSymbol argument: aParameter [
	^ self receiver: anObject selector: aSymbol arguments: (Array with: aParameter)
]

{ #category : #'instance creation' }
MessageSend class >> receiver: anObject selector: aSymbol arguments: anArray [
	^ self new
		receiver: anObject;
		selector: aSymbol;
		arguments: anArray
]

{ #category : #comparing }
MessageSend >> = anObject [
	^ anObject species == self species 
		and: [receiver == anObject receiver
		and: [selector == anObject selector
		and: [arguments = anObject arguments]]]
]

{ #category : #accessing }
MessageSend >> arguments [
	^ arguments
]

{ #category : #accessing }
MessageSend >> arguments: anArray [
	arguments := anArray
]

{ #category : #converting }
MessageSend >> asMinimalRepresentation [
	^self
]

{ #category : #converting }
MessageSend >> asWeakMessageSend [

	^ WeakMessageSend
		receiver: receiver
		selector: selector
		arguments: arguments copy
]

{ #category : #private }
MessageSend >> collectArguments: anArgArray [
	"Private"

    | staticArgs |
    staticArgs := self arguments.
    ^(anArgArray size = staticArgs size)
        ifTrue: [anArgArray]
        ifFalse:
            [(staticArgs isEmpty
                ifTrue: [ staticArgs := Array new: selector numArgs]
                ifFalse: [staticArgs copy] )
                    replaceFrom: 1
                    to: (anArgArray size min: staticArgs size)
                    with: anArgArray
                    startingAt: 1]
]

{ #category : #evaluating }
MessageSend >> cull: arg [
	^ selector numArgs = 0 
		ifTrue: [ self value ]
		ifFalse: [ self value: arg ].

	
]

{ #category : #evaluating }
MessageSend >> cull: arg1 cull: arg2 [
	^ selector numArgs < 2 
		ifTrue: [ self cull: arg1]
		ifFalse: [ self value: arg1 value: arg2 ]
]

{ #category : #evaluating }
MessageSend >> cull: arg1 cull: arg2 cull: arg3 [
	^ selector numArgs < 3 
		ifTrue: [ self cull: arg1 cull: arg2 ]
		ifFalse: [ self value: arg1 value: arg2 value: arg3 ]
]

{ #category : #comparing }
MessageSend >> hash [
	^ receiver hash bitXor: selector hash
]

{ #category : #testing }
MessageSend >> isMessageSend [
	^true

]

{ #category : #testing }
MessageSend >> isValid [
	^true
]

{ #category : #accessing }
MessageSend >> message [
	^Message selector: selector arguments: arguments
]

{ #category : #accessing }
MessageSend >> numArgs [
	"Answer the number of arguments in this message"

	^arguments size
]

{ #category : #printing }
MessageSend >> printOn: aStream [

        aStream
                nextPutAll: self class name;
                nextPut: $(.
        selector printOn: aStream.
        aStream nextPutAll: ' -> '.
        receiver printOn: aStream.
        aStream nextPut: $)
]

{ #category : #accessing }
MessageSend >> receiver [
	^ receiver
]

{ #category : #accessing }
MessageSend >> receiver: anObject [
	receiver := anObject
]

{ #category : #accessing }
MessageSend >> selector [
	^ selector
]

{ #category : #accessing }
MessageSend >> selector: aSymbol [
	selector := aSymbol
]

{ #category : #evaluating }
MessageSend >> value [
  "Send the message and answer the return value"

  arguments ifNil: [^ receiver perform: selector].

  ^ receiver 
    perform: selector 
    withArguments: (self collectArguments: arguments)
]

{ #category : #evaluating }
MessageSend >> value: anObject [

  ^ receiver 
    perform: selector 
    with: anObject
]

{ #category : #evaluating }
MessageSend >> value: anObject1 value: anObject2 [

	^ receiver 
		perform: selector 
		with: anObject1
		with: anObject2
]

{ #category : #evaluating }
MessageSend >> value: anObject1 value: anObject2 value: anObject3 [

	^ receiver 
		perform: selector 
		with: anObject1
		with: anObject2
		with: anObject3
]

{ #category : #evaluating }
MessageSend >> valueWithArguments: anArray [

	^ receiver 
		perform: selector 
		withArguments: (self collectArguments: anArray)
]

{ #category : #evaluating }
MessageSend >> valueWithEnoughArguments: anArray [
	"call the selector with enough arguments from arguments and anArray"
	| args |
	args := Array new: selector numArgs.
	args replaceFrom: 1
		to: (arguments size min: args size)
		with: arguments
		startingAt: 1.
	args size > arguments size ifTrue: [
		args replaceFrom: arguments size + 1
			to: (arguments size + anArray size min: args size)
			with: anArray
			startingAt: 1.
	].
	^ receiver perform: selector withArguments: args
]
