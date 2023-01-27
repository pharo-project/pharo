"
I am an abstract class that represents an accessor for a sequence of objects. This sequence is referred to as my ""contents"".
"
Class {
	#name : #Stream,
	#superclass : #Object,
	#category : #'Collections-Streams-Base'
}

{ #category : #testing }
Stream class >> isAbstract [

	^ self == Stream
]

{ #category : #'instance creation' }
Stream class >> new [

	self error: 'Streams are created with on: and with:'
]

{ #category : #accessing }
Stream >> << anObject [
	"Write anObject to the receiver, dispatching using #putOn:
	This is a shortcut for both nextPut: and nextPutAll: since anObject can be both
	the element type of the receiver as well as a collection of those elements.
	No further conversions of anObject are applied.
	Return self to accomodate chaining."

 	anObject putOn: self
]

{ #category : #testing }
Stream >> atEnd [
	"Answer whether the receiver can access any more objects."

	self subclassResponsibility
]

{ #category : #private }
Stream >> basicNext [

	^ self next
]

{ #category : #private }
Stream >> basicNext: anAmount putAll: aCollection startingAt: startIndex [

	^ self next: anAmount putAll: aCollection startingAt: startIndex
]

{ #category : #private }
Stream >> basicNextPut: anObject [

	^ self nextPut: anObject
]

{ #category : #private }
Stream >> basicNextPutAll: aCollection [

	^ self nextPutAll: aCollection
]

{ #category : #modes }
Stream >> binary [
	"do nothing"
	^self
]

{ #category : #'open/close' }
Stream >> close [
	"Presumably sets the status of the receiver to be closed. This message does
	nothing at this level, but is included for FileStream compatibility."

	^self
]

{ #category : #testing }
Stream >> closed [
	^ false
]

{ #category : #accessing }
Stream >> contents [
	"Answer all of the contents of the receiver."

	self subclassResponsibility
]

{ #category : #enumerating }
Stream >> do: aBlock [
	"Evaluate aBlock for each of the objects accessible by receiver."

	[self atEnd]
		whileFalse: [aBlock value: self next]
]

{ #category : #flushing }
Stream >> flush [
	"Do nothing by default"
]

{ #category : #testing }
Stream >> isBinary [
	^false
]

{ #category : #testing }
Stream >> isStream [
	"Return true if the receiver responds to the stream protocol"
	^true
]

{ #category : #accessing }
Stream >> localName [
	^'a stream'
]

{ #category : #accessing }
Stream >> next [
	"Answer the next object accessible by the receiver."

	self subclassResponsibility
]

{ #category : #accessing }
Stream >> next: anInteger [
	"Answer the next anInteger number of objects accessible by the receiver."

	| aCollection |
	aCollection := OrderedCollection new.
	anInteger timesRepeat: [aCollection addLast: self next].
	^aCollection
]

{ #category : #accessing }
Stream >> next: anInteger put: anObject [
	<compilerOptions: #(+ optionInlineTimesRepeat)>

	"Make anObject be the next anInteger number of objects accessible by the
	receiver. Answer anObject."

	anInteger timesRepeat: [ self nextPut: anObject ].
	^ anObject
]

{ #category : #accessing }
Stream >> next: anInteger putAll: aCollection startingAt: startIndex [
	"Store the next anInteger elements from the given collection."
	(startIndex = 1 and:[anInteger = aCollection size])
		ifTrue:[^self nextPutAll: aCollection].
	^self nextPutAll: (aCollection copyFrom: startIndex to: startIndex+anInteger-1)
]

{ #category : #accessing }
Stream >> nextMatchAll: aColl [
    "Answer true if next N objects are the ones in aColl,
     else false.  Advance stream of true, leave as was if false."
    | save |
    save := self position.
    aColl do: [:each |
       (self next) = each ifFalse: [
            self position: save.
            ^ false]
        ].
    ^ true
]

{ #category : #accessing }
Stream >> nextMatchFor: anObject [
	"Gobble the next object and answer whether it is equal to the argument,
	anObject."

	^anObject = self next
]

{ #category : #accessing }
Stream >> nextPut: anObject [
	"Insert the argument, anObject, as the next object accessible by the
	receiver. Answer anObject."

	self subclassResponsibility
]

{ #category : #accessing }
Stream >> nextPutAll: aCollection [
	"Append the elements of aCollection to the sequence of objects accessible
	by the receiver. Answer aCollection."

	aCollection do: [:v | self nextPut: v].
	^aCollection
]

{ #category : #private }
Stream >> nextWordsPutAll: aCollection [
	"Write the argument a word-like object in big endian format on the receiver.
	May be used to write other than plain word-like objects (such as ColorArray)."

	(aCollection class isPointers or: [ aCollection class isWords not ])
		ifTrue: [ ^ self error: aCollection class name,' is not word-like'].
	1 to: aCollection basicSize do: [ :i |
		self nextNumber: 4 put: (aCollection basicAt: i) ].
	^ aCollection
]

{ #category : #'open/close' }
Stream >> openReadOnly [
	^self
]

{ #category : #printing }
Stream >> print: anObject [
	"Have anObject print itself on the receiver."

	anObject printOn: self
]

{ #category : #modes }
Stream >> readOnly [
	"do nothing"
	^self
]

{ #category : #accessing }
Stream >> upToEnd [
	"answer the remaining elements in the string"
	| elements |
	elements := OrderedCollection new.
	[ self atEnd ] whileFalse: [
		elements add: self next ].
	^elements
]
