"
I am designed specifically for dictionaries.
"
Class {
	#name : #DictionaryValueHolder,
	#superclass : #NewValueHolder,
	#category : 'NewValueHolder'
}

{ #category : #override }
DictionaryValueHolder >> at: anObject [

	^ value at: anObject
]

{ #category : #protocol }
DictionaryValueHolder >> at: key ifAbsentPut: aBlock [ 

	^ value at: key ifAbsent: [self at: key put: aBlock value]
]

{ #category : #protocol }
DictionaryValueHolder >> at: key put: anObject [

	value at: key put: anObject.
	self valueChanged: anObject.
	^ anObject
]

{ #category : #protocol }
DictionaryValueHolder >> atNewIndex: index put: anObject [
	
	value atNewIndex: index put: anObject.
	self valueChanged: anObject.
]

{ #category : #protocol }
DictionaryValueHolder >> doesNotUnderstand: aMessage [

	^ (value respondsTo: aMessage selector)
		ifTrue: [ value perform: aMessage selector withEnoughArguments: aMessage arguments ]
		ifFalse: [ super doesNotUnderstand: aMessage ]
]

{ #category : #protocol }
DictionaryValueHolder >> fillFrom: aCollection with: aBlock [
	"Evaluate aBlock with each of aCollections's elements as the argument.  
	Collect the resulting values into self. Answer self."

	aCollection keysAndValuesDo: [ :key :aValue |
		self at: key put: (aBlock value: aValue) ]
]

{ #category : #initialization }
DictionaryValueHolder >> initialize [

	super initialize.
	
	self value: Dictionary new.
]

{ #category : #initialization }
DictionaryValueHolder >> initialize: n [
	
	value initialize: n.
	self valueChanged.
]

{ #category : #protocol }
DictionaryValueHolder >> removeAll [
	
	value removeAll.
	self valueChanged.
]

{ #category : #protocol }
DictionaryValueHolder >> removeKey: key [ 
	"Remove key from the receiver.
	If key is not in the receiver, notify an error."
	
	^ self removeKey: key ifAbsent: [self errorKeyNotFound: key].
]

{ #category : #protocol }
DictionaryValueHolder >> removeKey: key ifAbsent: aBlock [ 
	| result |
	
	result := value removeKey: key ifAbsent: aBlock.
	self valueChanged.
	^ result
]

{ #category : #override }
DictionaryValueHolder >> size [ 

	^ value size
]

{ #category : #protocol }
DictionaryValueHolder >> valueAtNewKey: aKey put: anObject atIndex: index declareFrom: aDictionary [ 
	
	value valueAtNewKey: aKey put: anObject atIndex: index declareFrom: aDictionary.
	self valueChanged.
]
