"
I represent a selector and its argument values.

Generally, the system does not use instances of Message for efficiency reasons. However, when a message is not understood by its receiver, the interpreter will make up an instance of me in order to capture the information involved in an actual message transmission. This instance is sent it as an argument with the message doesNotUnderstand: to the receiver.

## Structure

- selector: `Symbol` -- message selector
- arguments: `Array` -- bound arguments
"
Class {
	#name : 'Message',
	#superclass : 'Object',
	#instVars : [
		'selector',
		'arguments',
		'lookupClass'
	],
	#category : 'Kernel-CodeModel-Methods',
	#package : 'Kernel-CodeModel',
	#tag : 'Methods'
}

{ #category : 'instance creation' }
Message class >> selector: aSymbol [
	"Answer an instance of me with unary selector, aSymbol."
	<reflection: 'Message sending and code execution - Message send reification'>
	^self new setSelector: aSymbol arguments: #()
]

{ #category : 'instance creation' }
Message class >> selector: aSymbol argument: anObject [
	"Answer an instance of me whose selector is aSymbol and single argument is anObject."

	^self new setSelector: aSymbol arguments: { anObject }
]

{ #category : 'instance creation' }
Message class >> selector: aSymbol arguments: anArray [
	"Answer an instance of me with selector, aSymbol, and arguments, anArray."

	^self new setSelector: aSymbol arguments: anArray
]

{ #category : 'comparing' }
Message >> analogousCodeTo: anObject [
	"For MethodPropertires comparison."
	^self class == anObject class
	  and: [selector == anObject selector
	  and: [arguments = anObject arguments
	  and: [lookupClass == anObject lookupClass]]]
]

{ #category : 'accessing' }
Message >> argument [
	"Answer the first (presumably sole) argument"
	<reflection: 'Message sending and code execution - Message send reification'>
	^arguments at: 1
]

{ #category : 'accessing' }
Message >> argument: newValue [
	"Change the first argument to newValue and answer self"
	<reflection: 'Class structural inspection - Shared pool inspection'>
	arguments at: 1 put: newValue
]

{ #category : 'accessing' }
Message >> arguments [
	"Answer the arguments of the receiver."
	<reflection: 'Message sending and code execution - Message send reification'>
	^arguments
]

{ #category : 'accessing' }
Message >> arguments: anArray [
	<reflection: 'Message sending and code execution - Message send reification'>
	arguments := anArray
]

{ #category : 'converting' }
Message >> asSendTo: anObject [

	<reflection: 'Message sending and code execution - Message send reification'>
	^MessageSend message: self to: anObject
]

{ #category : 'testing' }
Message >> hasArguments [
	<reflection: 'Message sending and code execution - Message send reification'>
	^arguments notEmpty
]

{ #category : 'accessing' }
Message >> lookupClass [
	<reflection: 'Message sending and code execution - Message send reification'>
	^ lookupClass
]

{ #category : 'private' }
Message >> lookupClass: aClass [
	<reflection: 'Message sending and code execution - Message send reification'>
	lookupClass := aClass
]

{ #category : 'accessing' }
Message >> message [
	^self
]

{ #category : 'accessing' }
Message >> numArgs [
	"Answer the number of arguments in this message"
	<reflection: 'Message sending and code execution - Message send reification'>
	^arguments size
]

{ #category : 'printing' }
Message >> printOn: stream [

	arguments ifEmpty: [ ^ stream nextPutAll: selector ].
	arguments with: selector keywords do: [ :arg :word |
		stream nextPutAll: word.
		stream space.
		arg printOn: stream.
		stream space ].
	stream skip: -1
]

{ #category : 'accessing' }
Message >> selector [
	"Answer the selector of the receiver."
	<reflection: 'Message sending and code execution - Message send reification'>
	^selector
]

{ #category : 'accessing' }
Message >> selector: aSymbol [
	<reflection: 'Message sending and code execution - Message send reification'>
	selector := aSymbol
]

{ #category : 'sending' }
Message >> sendTo: receiver [
	"answer the result of sending this message to receiver"
	<reflection: 'Message sending and code execution - Reflective message send'>
	^ receiver perform: selector withArguments: arguments
]

{ #category : 'accessing' }
Message >> sends: aSelector [
	"answer whether this message's selector is aSelector"
	<reflection: 'Message sending and code execution - Message send reification'>
	^selector == aSelector
]

{ #category : 'sending' }
Message >> sentTo: receiver [
	"answer the result of sending this message to receiver"
	<reflection: 'Message sending and code execution - Runtime and Evaluation'>
	^ lookupClass
		ifNil: [ receiver perform: selector withArguments: arguments]
		ifNotNil: [ receiver perform: selector withArguments: arguments inSuperclass: lookupClass]
]

{ #category : 'private' }
Message >> setSelector: aSymbol [
	<reflection: 'Message sending and code execution - Message send reification'>
	selector := aSymbol
]

{ #category : 'private' }
Message >> setSelector: aSymbol arguments: anArray [
	<reflection: 'Message sending and code execution - Message send reification'>
	selector := aSymbol.
	arguments := anArray
]

{ #category : 'storing' }
Message >> storeOn: aStream [
	"Refer to the comment in Object|storeOn:."

	aStream nextPut: $(;
	 nextPutAll: self class name;
	 nextPutAll: ' selector: ';
	 store: selector;
	 nextPutAll: ' arguments: ';
	 store: arguments;
	 nextPut: $)
]
