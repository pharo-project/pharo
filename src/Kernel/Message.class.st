"
I represent a selector and its argument values.
	
Generally, the system does not use instances of Message for efficiency reasons. However, when a message is not understood by its receiver, the interpreter will make up an instance of me in order to capture the information involved in an actual message transmission. This instance is sent it as an argument with the message doesNotUnderstand: to the receiver.
"
Class {
	#name : #Message,
	#superclass : #Object,
	#instVars : [
		'selector',
		'arguments',
		'lookupClass'
	],
	#category : #'Kernel-Methods'
}

{ #category : #'instance creation' }
Message class >> selector: aSymbol [
	"Answer an instance of me with unary selector, aSymbol."

	^self new setSelector: aSymbol arguments: (Array new: 0)
]

{ #category : #'instance creation' }
Message class >> selector: aSymbol argument: anObject [
	"Answer an instance of me whose selector is aSymbol and single
	argument is anObject."

	^self new setSelector: aSymbol arguments: { anObject }
]

{ #category : #'instance creation' }
Message class >> selector: aSymbol arguments: anArray [
	"Answer an instance of me with selector, aSymbol, and arguments,
	anArray."

	^self new setSelector: aSymbol arguments: anArray
]

{ #category : #comparing }
Message >> analogousCodeTo: anObject [
	"For MethodPropertires comparison."
	^self class == anObject class
	  and: [selector == anObject selector
	  and: [arguments = anObject arguments
	  and: [lookupClass == anObject lookupClass]]]
]

{ #category : #accessing }
Message >> argument [
	"Answer the first (presumably sole) argument"

	^arguments at: 1
]

{ #category : #accessing }
Message >> argument: newValue [
	"Change the first argument to newValue and answer self"

	arguments at: 1 put: newValue
]

{ #category : #accessing }
Message >> arguments [
	"Answer the arguments of the receiver."

	^arguments
]

{ #category : #converting }
Message >> asSendTo: anObject [
	^MessageSend message: self to: anObject
]

{ #category : #testing }
Message >> hasArguments [
	^arguments notEmpty
]

{ #category : #accessing }
Message >> lookupClass [

	^ lookupClass
]

{ #category : #private }
Message >> lookupClass: aClass [

	lookupClass := aClass
]

{ #category : #accessing }
Message >> numArgs [
	"Answer the number of arguments in this message"

	^arguments size
]

{ #category : #printing }
Message >> printOn: stream [

	arguments ifEmpty: [ ^ stream nextPutAll: selector ].
	arguments with: selector keywords do: [ :arg :word |
		stream nextPutAll: word.
		stream space.
		arg printOn: stream.
		stream space ].
	stream skip: -1
]

{ #category : #accessing }
Message >> selector [
	"Answer the selector of the receiver."

	^selector
]

{ #category : #sending }
Message >> sendTo: receiver [
	"answer the result of sending this message to receiver"

	^ receiver perform: selector withArguments: arguments
]

{ #category : #accessing }
Message >> sends: aSelector [
	"answer whether this message's selector is aSelector"

	^selector == aSelector
]

{ #category : #sending }
Message >> sentTo: receiver [
	"answer the result of sending this message to receiver"

	^ lookupClass
		ifNil: [ receiver perform: selector withArguments: arguments]
		ifNotNil: [ receiver perform: selector withArguments: arguments inSuperclass: lookupClass]
]

{ #category : #private }
Message >> setSelector: aSymbol [

	selector := aSymbol
]

{ #category : #private }
Message >> setSelector: aSymbol arguments: anArray [

	selector := aSymbol.
	arguments := anArray
]

{ #category : #printing }
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
