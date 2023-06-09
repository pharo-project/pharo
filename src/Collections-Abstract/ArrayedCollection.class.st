"
I am an abstract collection of elements with a fixed range of integers (from 1 to n>=0) as external keys.
"
Class {
	#name : #ArrayedCollection,
	#superclass : #SequenceableCollection,
	#category : #'Collections-Abstract-Base'
}

{ #category : #testing }
ArrayedCollection class >> isAbstract [

	^self name = #ArrayedCollection
]

{ #category : #'instance creation' }
ArrayedCollection class >> new [
	"Answer a new instance of me, with size = 0."

	^self new: 0
]

{ #category : #'instance creation' }
ArrayedCollection class >> new: size withAll: value [
	"Answer an instance of me, with number of elements equal to size, each
	of which refers to the argument, value."

	^(self new: size) atAllPut: value
]

{ #category : #'instance creation' }
ArrayedCollection class >> newFrom: aCollection [
	"Answer an instance of me containing the same elements as aCollection."
	| newArray |
	newArray := self new: aCollection size.
	1 to: aCollection size do: [:i | newArray at: i put: (aCollection at: i)].
	^ newArray

"	Array newFrom: {1. 2. 3}
	{1. 2. 3} as: Array
	{1. 2. 3} as: ByteArray
	{$c. $h. $r} as: String
	{$c. $h. $r} as: Text
"
]

{ #category : #'instance creation' }
ArrayedCollection class >> newFromArray: anArray [

	| newCollection size |
	size := anArray size.
	newCollection := self new: size.
	1 to: size do: [ :i | newCollection at: i put: (anArray at: i) ].
	^ newCollection
]

{ #category : #'instance creation' }
ArrayedCollection class >> with: anObject [
	"Answer a new instance of me, containing only anObject."

	| newCollection |
	newCollection := self new: 1.
	newCollection at: 1 put: anObject.
	^newCollection
]

{ #category : #'instance creation' }
ArrayedCollection class >> with: firstObject with: secondObject [
	"Answer a new instance of me, containing firstObject and secondObject."

	| newCollection |
	newCollection := self new: 2.
	newCollection at: 1 put: firstObject.
	newCollection at: 2 put: secondObject.
	^newCollection
]

{ #category : #'instance creation' }
ArrayedCollection class >> with: firstObject with: secondObject with: thirdObject [
	"Answer a new instance of me, containing only the three arguments as
	elements."

	| newCollection |
	newCollection := self new: 3.
	newCollection at: 1 put: firstObject.
	newCollection at: 2 put: secondObject.
	newCollection at: 3 put: thirdObject.
	^newCollection
]

{ #category : #'instance creation' }
ArrayedCollection class >> with: firstObject with: secondObject with: thirdObject with: fourthObject [
	"Answer a new instance of me, containing only the three arguments as
	elements."

	| newCollection |
	newCollection := self new: 4.
	newCollection at: 1 put: firstObject.
	newCollection at: 2 put: secondObject.
	newCollection at: 3 put: thirdObject.
	newCollection at: 4 put: fourthObject.
	^newCollection
]

{ #category : #'instance creation' }
ArrayedCollection class >> with: firstObject with: secondObject with: thirdObject with: fourthObject with: fifthObject [
	"Answer a new instance of me, containing only the five arguments as
	elements."

	| newCollection |
	newCollection := self new: 5.
	newCollection at: 1 put: firstObject.
	newCollection at: 2 put: secondObject.
	newCollection at: 3 put: thirdObject.
	newCollection at: 4 put: fourthObject.
	newCollection at: 5 put: fifthObject.
	^newCollection
]

{ #category : #'instance creation' }
ArrayedCollection class >> with: firstObject with: secondObject with: thirdObject with: fourthObject with: fifthObject with: sixthObject [
	"Answer a new instance of me, containing only the 6 arguments as elements."

	| newCollection |
	newCollection := self new: 6.
	newCollection at: 1 put: firstObject.
	newCollection at: 2 put: secondObject.
	newCollection at: 3 put: thirdObject.
	newCollection at: 4 put: fourthObject.
	newCollection at: 5 put: fifthObject.
	newCollection at: 6 put: sixthObject.
	^ newCollection
]

{ #category : #'instance creation' }
ArrayedCollection class >> with: anObject withAll: aCollection [
	"Creates an array with anObject followed by elements contained in aCollection."
	"(Array with: 1 withAll: #(2 3 4)) >>> #(1 2 3 4)"

	| newArray |
	newArray := self new: aCollection size + 1.
	newArray at: 1 put: anObject.
	newArray
		replaceFrom: 2
		to: newArray size
		with: aCollection.
	^newArray
]

{ #category : #'instance creation' }
ArrayedCollection class >> withAll: aCollection [
	"Create a new collection containing all the elements from aCollection."

	^ (self new: aCollection size) replaceFrom: 1 to: aCollection size with: aCollection
]

{ #category : #adding }
ArrayedCollection >> add: newObject [
	self shouldNotImplement
]

{ #category : #accessing }
ArrayedCollection >> byteSize [
	^self basicSize * self bytesPerBasicElement
]

{ #category : #'objects from disk' }
ArrayedCollection >> bytesPerBasicElement [
	"Answer the number of bytes that each of my basic elements requires.
	In other words:
		self basicSize * self bytesPerBasicElement
	should equal the space required on disk by my variable sized representation."
	^self class isBytes ifTrue: [ 1 ] ifFalse: [ 4 ]
]

{ #category : #private }
ArrayedCollection >> defaultElement [

	^nil
]

{ #category : #private }
ArrayedCollection >> fillFrom: aCollection with: aBlock [
	"Evaluate aBlock with each of aCollections's elements as the argument.
	Collect the resulting values into self. Answer self."

	| index |
	index := 0.
	aCollection do: [ :each |
		self at: (index := index + 1) put: (aBlock value: each) ]
]

{ #category : #removing }
ArrayedCollection >> removeAll [

	self shouldNotImplement
]

{ #category : #accessing }
ArrayedCollection >> size [
	"Answer how many elements the receiver contains."

	<primitive: 62>
	^ self basicSize
]

{ #category : #private }
ArrayedCollection >> storeElementsFrom: firstIndex to: lastIndex on: aStream [

	| noneYet defaultElement arrayElement |
	noneYet := true.
	defaultElement := self defaultElement.
	firstIndex to: lastIndex do:
		[:index |
		arrayElement := self at: index.
		arrayElement = defaultElement
			ifFalse:
				[noneYet
					ifTrue: [noneYet := false]
					ifFalse: [aStream nextPut: $;].
				aStream nextPutAll: ' at: '.
				aStream store: index.
				aStream nextPutAll: ' put: '.
				aStream store: arrayElement]].
	^noneYet
]

{ #category : #printing }
ArrayedCollection >> storeOn: aStream [

	aStream nextPutAll: '(('.
	aStream nextPutAll: self class name.
	aStream nextPutAll: ' new: '.
	aStream store: self size.
	aStream nextPut: $).
	(self storeElementsFrom: 1 to: self size on: aStream)
		ifFalse: [aStream nextPutAll: '; yourself'].
	aStream nextPut: $)
]

{ #category : #'objects from disk' }
ArrayedCollection >> writeOn: aStream [
	"Store the array of bits onto the argument, aStream.  (leading byte ~= 16r80) identifies this as raw bits (uncompressed).  Always store in Big Endian (Mac) byte order.  Do the writing at BitBlt speeds. We only intend this for non-pointer arrays.  Do nothing if I contain pointers."
	(self class isPointers or: [ self class isWords not ]) ifTrue: [^ self ].
	aStream nextInt32Put: self basicSize.
	aStream nextWordsPutAll: self
]
