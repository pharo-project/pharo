"
Instances of `MessageSend` encapsulate messages send to objects. Arguments can be either predefined or supplied when the message send is performed. 

Use #value to perform a message send with its predefined arguments and #valueWithArguments: if additonal arguments have to supplied.

## Structure

- receiver: 	Object -- object receiving the message send

"
Class {
	#name : 'MessageSend',
	#superclass : 'Message',
	#instVars : [
		'receiver'
	],
	#category : 'Kernel-Messaging',
	#package : 'Kernel',
	#tag : 'Messaging'
}

{ #category : 'instance creation' }
MessageSend class >> message: aMessage to: anObject [
	^ self receiver: anObject selector: aMessage selector arguments: aMessage arguments
]

{ #category : 'instance creation' }
MessageSend class >> receiver: anObject selector: aSymbol [
	^ self receiver: anObject selector: aSymbol arguments: #()
]

{ #category : 'instance creation' }
MessageSend class >> receiver: anObject selector: aSymbol argument: aParameter [
	^ self receiver: anObject selector: aSymbol arguments: { aParameter }
]

{ #category : 'instance creation' }
MessageSend class >> receiver: anObject selector: aSymbol arguments: anArray [
	^ self new
		receiver: anObject;
		selector: aSymbol;
		arguments: anArray
]

{ #category : 'comparing' }
MessageSend >> = anObject [
	^ anObject species == self species
		and: [receiver == anObject receiver
		and: [selector == anObject selector
		and: [arguments = anObject arguments]]]
]

{ #category : 'converting' }
MessageSend >> asMinimalRepresentation [
	^self
]

{ #category : 'converting' }
MessageSend >> asWeakMessageSend [

	^ WeakMessageSend
		receiver: receiver
		selector: selector
		arguments: arguments copy
]

{ #category : 'private' }
MessageSend >> collectArguments: anArgArray [
	"Private"
	<reflection: 'Message sending and code execution - Message send reification'>
	| staticArgs |
	staticArgs := self arguments.
	^ anArgArray size = staticArgs size
		  ifTrue: [ anArgArray ]
		  ifFalse: [
			  (staticArgs ifEmpty: [ staticArgs := Array new: selector numArgs ] ifNotEmpty: [ staticArgs copy ])
				  replaceFrom: 1
				  to: (anArgArray size min: staticArgs size)
				  with: anArgArray
				  startingAt: 1 ]
]

{ #category : 'evaluating' }
MessageSend >> cull: arg [
	<reflection: 'Message sending and code execution - Runtime and Evaluation'>
	^ selector numArgs = 0
		ifTrue: [ self value ]
		ifFalse: [ self value: arg ]
]

{ #category : 'evaluating' }
MessageSend >> cull: arg1 cull: arg2 [
	<reflection: 'Message sending and code execution - Runtime and Evaluation'>
	^ selector numArgs < 2
		ifTrue: [ self cull: arg1]
		ifFalse: [ self value: arg1 value: arg2 ]
]

{ #category : 'evaluating' }
MessageSend >> cull: arg1 cull: arg2 cull: arg3 [
	<reflection: 'Message sending and code execution - Runtime and Evaluation'>
	^ selector numArgs < 3
		ifTrue: [ self cull: arg1 cull: arg2 ]
		ifFalse: [ self value: arg1 value: arg2 value: arg3 ]
]

{ #category : 'comparing' }
MessageSend >> hash [
	^ receiver hash bitXor: selector hash
]

{ #category : 'testing' }
MessageSend >> isMessageSend [
	^true
]

{ #category : 'testing' }
MessageSend >> isValid [
	<reflection: 'Message sending and code execution - Message send reification'>
	^true
]

{ #category : 'accessing' }
MessageSend >> message [
	<reflection: 'Message sending and code execution - Message send reification'>
	^Message selector: selector arguments: arguments
]

{ #category : 'printing' }
MessageSend >> printOn: aStream [

        aStream
                nextPutAll: self class name;
                nextPut: $(.
        selector printOn: aStream.
        aStream nextPutAll: ' -> '.
        receiver printOn: aStream.
        aStream nextPut: $)
]

{ #category : 'accessing' }
MessageSend >> receiver [
	<reflection: 'Message sending and code execution - Message send reification'>
	^ receiver
]

{ #category : 'accessing' }
MessageSend >> receiver: anObject [
	<reflection: 'Message sending and code execution - Message send reification'>
	receiver := anObject
]

{ #category : 'evaluating' }
MessageSend >> value [
  "Send the message and answer the return value"
	<reflection: 'Message sending and code execution - Runtime and Evaluation'>

  arguments ifNil: [^ receiver perform: selector].

  ^ receiver
    perform: selector
    withArguments: (self collectArguments: arguments)
]

{ #category : 'evaluating' }
MessageSend >> value: anObject [
	<reflection: 'Message sending and code execution - Runtime and Evaluation'>
  ^ receiver
    perform: selector
    with: anObject
]

{ #category : 'evaluating' }
MessageSend >> value: anObject1 value: anObject2 [
	<reflection: 'Message sending and code execution - Runtime and Evaluation'>
	^ receiver
		perform: selector
		with: anObject1
		with: anObject2
]

{ #category : 'evaluating' }
MessageSend >> value: anObject1 value: anObject2 value: anObject3 [
	<reflection: 'Message sending and code execution - Runtime and Evaluation'>
	^ receiver
		perform: selector
		with: anObject1
		with: anObject2
		with: anObject3
]

{ #category : 'evaluating' }
MessageSend >> valueWithArguments: anArray [
	<reflection: 'Message sending and code execution - Runtime and Evaluation'>
	^ receiver
		perform: selector
		withArguments: (self collectArguments: anArray)
]

{ #category : 'evaluating' }
MessageSend >> valueWithEnoughArguments: anArray [
	"call the selector with enough arguments from arguments and anArray"
	<reflection: 'Message sending and code execution - Runtime and Evaluation'>
	| newArgs |
	newArgs := Array new: selector numArgs.
	newArgs replaceFrom: 1
		to: (arguments size min: newArgs size)
		with: arguments
		startingAt: 1.
	newArgs size > arguments size ifTrue: [
		newArgs replaceFrom: arguments size + 1
			to: (arguments size + anArray size min: newArgs size)
			with: anArray
			startingAt: 1.
	].
	^ receiver perform: selector withArguments: newArgs
]
