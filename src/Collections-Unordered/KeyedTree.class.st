"
Provides path based access to elements contained in the receiver and any subtrees.

### Example
```
(KeyedTree new
	at: 1 put: 'One';
	at: 2 put: 'Two';
	at: 'Tree' put: (KeyedTree new
					at: $a put: 'Tree-A';
					at: $b put: 'Tree-B';
					yourself);
	yourself) atPath: #('Tree' $b)
```
"
Class {
	#name : #KeyedTree,
	#superclass : #Dictionary,
	#category : #'Collections-Unordered-Dictionaries'
}

{ #category : #accessing }
KeyedTree >> allKeys [
	"Answer an ordered collection of the keys of the receiver and any subtrees.
		Please no circular references!"

	|answer|
	answer := OrderedCollection new.
	answer addAll: self keys.
	self subtrees do: [:t |
		answer addAll: t allKeys].
	^answer
]

{ #category : #accessing }
KeyedTree >> atPath: anArray [
	"Answer the element referenced by the give key path.
	Signal an error if not found."

	^self atPath: anArray ifAbsent: [self errorKeyNotFound: anArray]
]

{ #category : #accessing }
KeyedTree >> atPath: anArray ifAbsent: aBlock [
	"Answer the element referenced by the given key path.
	Answer the value of aBlock if not found."

	|element|
	element := self.
	anArray do: [:key |
		element := element at: key ifAbsent: [^aBlock value]].
	^element
]

{ #category : #accessing }
KeyedTree >> atPath: anArray ifAbsentPut: aBlock [
	"Answer the element referenced by the given key path.
	Answer the value of aBlock if not found after creating its path."

	|element|
	anArray isEmpty
		ifTrue: [^self].
	element := self.
	anArray allButLastDo: [:key |
		element := element at: key ifAbsentPut: [self species new]].
	^element at: anArray last ifAbsentPut: aBlock
]

{ #category : #accessing }
KeyedTree >> atPath: anArray put: aBlock [
	"Answer the value of aBlock after creating its path."

	|element|
	anArray isEmpty
		ifTrue: [^self].
	element := self.
	anArray allButLastDo: [:key |
		element := element at: key ifAbsentPut: [self species new]].
	^element at: anArray last put: aBlock
]

{ #category : #printing }
KeyedTree >> formattedText [
	"Answer a string or text representing the receiver with indentation and, possibly, markup."

	|str|
	str := String new writeStream.
	self putFormattedTextOn: str level: 0 indentString: '  '.
	^str contents
]

{ #category : #printing }
KeyedTree >> formattedTextWithDescriptions: aKeyedTree [
	"Answer a string or text representing the receiver with indentation and, possibly, markup.
	Descriptions of each item are taken from the given tree with
	the same key structure as the receiver."

	|str|
	str := String new writeStream.
	self putFormattedTextOn: str withDescriptions: aKeyedTree level: 0 indentString: '  '.
	^str contents
]

{ #category : #adding }
KeyedTree >> merge: aKeyedTree [
	"Merge the given tree into the receiver, overwriting or extending elements as needed."

	aKeyedTree keysAndValuesDo: [ :k :v |
			| subtree |
			(v isKindOf: KeyedTree)
				ifTrue: [
					subtree := self at: k ifAbsentPut: [ v species new ].
					(subtree isKindOf: KeyedTree)
						ifFalse: [ subtree := self at: k put: v species new ].
					subtree merge: v ]
				ifFalse: [ self at: k put: v ] ]
]

{ #category : #copying }
KeyedTree >> postCopy [
	"Must copy the associations, or later store will affect both the
		original and the copy.
	Copy any subtrees too!"

	array := array
		collect: [ :assoc |
			assoc
				ifNotNil: [ Association
						key: assoc key
						value:
							((assoc value isKindOf: KeyedTree)
								ifTrue: [ assoc value copy ]
								ifFalse: [ assoc value ]) ] ]
]

{ #category : #printing }
KeyedTree >> putFormattedTextOn: aStream level: indentLevel indentString: aString [
	"Place a description of the receiver on the given stream with the given indentation level."


	(self keys asSortedCollection: self sortBlock) do: [:k | | v |
		indentLevel timesRepeat: [aStream nextPutAll: aString].
		aStream nextPutAll: k printString.
		v := self at: k.
		(v isKindOf: self class)
			ifTrue: [aStream cr.
					v putFormattedTextOn: aStream level: indentLevel + 1 indentString: aString]
			ifFalse: [aStream
						nextPutAll: ' : ';
						nextPutAll: v printString.
					aStream cr]]
]

{ #category : #printing }
KeyedTree >> putFormattedTextOn: aStream withDescriptions: aKeyedTree level: indentLevel indentString: aString [
	"Place a print of the receiver and associated description on the given stream with the given indentation level."


	(self keys asSortedCollection: self sortBlock) do: [:k | | v |
		indentLevel timesRepeat: [aStream nextPutAll: aString].
		aStream nextPutAll: k printString.
		v := self at: k.
		(v isKindOf: self class)
			ifTrue: [aStream cr.
					v
						putFormattedTextOn: aStream
						withDescriptions: (aKeyedTree at: k ifAbsent: [self class new])
						level: indentLevel + 1
						indentString: aString]
			ifFalse: [aStream
						nextPutAll: ' : ';
						nextPutAll: v printString;
						tab; tab;
						nextPutAll: (aKeyedTree at: k ifAbsent: ['nondescript']) printString.
					aStream cr]]
]

{ #category : #removing }
KeyedTree >> removePath: anArray [
	"Remove and answer the element referenced by the given path.
	Signal an error if not found."

	^self removePath: anArray ifAbsent: [self errorKeyNotFound: anArray]
]

{ #category : #removing }
KeyedTree >> removePath: anArray ifAbsent: aBlock [
	"Remove and answer the element referenced by the given path.
	Answer the value of aBlock if not found."

	|element|
	anArray isEmpty
		ifTrue: [^self].
	element := self.
	anArray allButLastDo: [:key |
		element := element at: key ifAbsent: [^aBlock value]].
	^element removeKey: anArray last ifAbsent: aBlock
]

{ #category : #accessing }
KeyedTree >> sortBlock [
	"Answer the block to sort tree keys with."

	^[:a :b | [a <= b] on: Error do: [a class name <= b class name]]
]

{ #category : #accessing }
KeyedTree >> subtrees [
	"Answer the subtrees of the receiver."

	^(self select: [:v | v isKindOf: KeyedTree]) values
]
