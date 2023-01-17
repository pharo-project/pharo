"
Array is a fixed-sized collection of elements accessed by integer indices. Contrary to the C convention, the first element of a Pharo array is at position 1 and not 0. The main protocol to access array elements is the method `at:` and `at:put:`. 

- `at: anInteger` returns the element at index anInteger. 
- `at: anInteger put: anObject` puts `anObject` at index `anInteger`. 

Arrays are fixed-size collections therefore we cannot add or remove elements at the end of an array. 

The following code creates an array of size 5, puts values in the first 3 locations and returns the first element.

``` 
| anArray |
anArray := Array new: 5. 
anArray at: 1 put: 4. 
anArray at: 2 put: 3/2. 
anArray at: 3 put: 'ssss'. 
anArray at: 1			
>>> 4
```

`#()` creates literal arrays with static (or “literal”) elements that have to be known when the expression is compiled, and not when it is executed. 

The following code creates an array of size 2 where the first element is the (literal) number 1 and the second the (literal) string 'here'.

``` 
#(1 'here') size 
> 2
```

`{ }` is a way to create arrays in a more dynamic manner.

``` 
| array |
array := (Array new: 2). 
array
	at: 1 put: (Point x: 10 y: 20);
	at: 2 put: (Point x: 10 y: 20). 
	array			
```

is equivalent to 

```
{(10@20) . (10@20)}
{Point x: 10 y: 20 . Point x: 10 y: 20}
```

"
Class {
	#name : #Array,
	#superclass : #ArrayedCollection,
	#type : #variable,
	#category : #'Collections-Sequenceable-Base'
}

{ #category : #'brace support' }
Array class >> braceStream: nElements [
	"This method is used in compilation of brace constructs.
	It MUST NOT be deleted or altered."

	^ WriteStream basicNew braceArray: (self new: nElements)
]

{ #category : #'brace support' }
Array class >> braceWith: a [
	"This method is used in compilation of brace constructs.
	It MUST NOT be deleted or altered."

	| array |
	array := self new: 1.
	array at: 1 put: a.
	^ array
]

{ #category : #'brace support' }
Array class >> braceWith: a with: b [
	"This method is used in compilation of brace constructs.
	It MUST NOT be deleted or altered."

	| array |
	array := self new: 2.
	array at: 1 put: a.
	array at: 2 put: b.
	^ array
]

{ #category : #'brace support' }
Array class >> braceWith: a with: b with: c [
	"This method is used in compilation of brace constructs.
	It MUST NOT be deleted or altered."

	| array |
	array := self new: 3.
	array at: 1 put: a.
	array at: 2 put: b.
	array at: 3 put: c.
	^ array
]

{ #category : #'brace support' }
Array class >> braceWith: a with: b with: c with: d [
	"This method is used in compilation of brace constructs.
	It MUST NOT be deleted or altered."

	| array |
	array := self new: 4.
	array at: 1 put: a.
	array at: 2 put: b.
	array at: 3 put: c.
	array at: 4 put: d.
	^ array
]

{ #category : #'brace support' }
Array class >> braceWithNone [
	"This method is used in compilation of brace constructs.
	It MUST NOT be deleted or altered."

	^ self new: 0
]

{ #category : #'instance creation' }
Array class >> empty [
	"A canonicalized empty Array instance."
	^ #()
]

{ #category : #'instance creation' }
Array class >> new: sizeRequested [
	"Answer an instance of this class with the number of indexable
	variables specified by the argument, sizeRequested.

	This is a shortcut (direct call of primitive, no #initialize, for performance"

	<primitive: 71>  "This method runs primitively if successful"
	^ self basicNew: sizeRequested  "Exceptional conditions will be handled in basicNew:"
]

{ #category : #converting }
Array >> as: aSimilarClass [

	"Specific classes can redefine newFromArray: by a faster implementation if they want.

	Note: see Collection class>>newFromArray: for the rationale."

	aSimilarClass == self class ifTrue: [ ^ self ].
	^ aSimilarClass newFromArray: self
]

{ #category : #converting }
Array >> asArray [
	"Answer with the receiver itself."

	^ self
]

{ #category : #converting }
Array >> asNewArray [

	^ self clone
]

{ #category : #converting }
Array >> asOrderedCollection [

	^ OrderedCollection newFromArray: self
]

{ #category : #converting }
Array >> asSet [

	^ Set newFromArray: self
]

{ #category : #accessing }
Array >> atWrap: index [

    "Answer the index'th element of the receiver.  If index is out of bounds,
    let it wrap around from the end to the beginning until it is in bounds."

    "Optimized to go through the primitive if possible"

    "(#(11 22 33) atWrap: 3) >>> 33"
    "(#(11 22 33) atWrap: 2) >>> 22"
    "(#(11 22 33) atWrap: 4) >>> 11"

    <primitive: 60>
    ^ self at: index - 1 \\ self size + 1
]

{ #category : #accessing }
Array >> atWrap: index put: anObject [
	"Optimized to go through the primitive if possible"
	<primitive: 61>
	^ self at: index - 1 \\ self size + 1 put: anObject
]

{ #category : #'write barrier' }
Array >> beReadOnlyLiteral [
	self isLiteral ifFalse: [ ^ self ].
	self beReadOnlyObject.
	self do: [ :each | each beReadOnlyLiteral ]
]

{ #category : #copying }
Array >> copyWithDependent: newElement [

	self isEmpty ifTrue: [ ^ DependentsArray with: newElement ].
	^ self copyWith: newElement
]

{ #category : #converting }
Array >> elementsExchangeIdentityWith: otherArray [
	"This primitive performs a bulk mutation, causing all pointers to the elements of the
	 receiver to be replaced by pointers to the corresponding elements of otherArray.
	 At the same time, all pointers to the elements of otherArray are replaced by
	 pointers to the corresponding elements of this array.  The identityHashes remain
	 with the pointers rather than with the objects so that objects in hashed structures
	 should still be properly indexed after the mutation."

	<primitive: 128 error: ec>
	| minimalRequiredSize |
	ec == #'bad receiver' ifTrue:
		[^self error: 'receiver must be of class Array'].
	ec == #'bad argument' ifTrue:
		[^self error: (otherArray class == Array
						ifTrue: ['arg must be of class Array']
						ifFalse: ['receiver and argument must have the same size'])].
	ec == #'inappropriate operation' ifTrue:
		[^self error: 'can''t become immediates such as SmallIntegers or Characters'].
	ec == #'no modification' ifTrue:
		[^self error: 'can''t become immutable objects'].
	ec == #'object is pinned' ifTrue:
		[^self error: 'can''t become pinned objects'].
	ec == #'insufficient object memory' ifTrue:
		[
			minimalRequiredSize :=
				(self sumNumbers: [ :anElement | anElement sizeInMemory ])
				+ (otherArray sumNumbers: [ :anElement | anElement sizeInMemory ]).

			Smalltalk garbageCollect < minimalRequiredSize ifTrue:
				[ (Smalltalk growMemoryByAtLeast: minimalRequiredSize) < minimalRequiredSize ifTrue:[
					self error: 'Could not allocate enought memory for two way become']].

		 ^self elementsExchangeIdentityWith: otherArray].
	self primitiveFailed
]

{ #category : #converting }
Array >> elementsForwardIdentityTo: otherArray [
	"This primitive performs a bulk mutation, causing all pointers to the elements of the
	 receiver to be replaced by pointers to the corresponding elements of otherArray.
	 The identityHashes remain with the pointers rather than with the objects so that
	 the objects in this array should still be properly indexed in any existing hashed
	 structures after the mutation."
	<primitive: 72 error: ec>
	self primitiveFailed
]

{ #category : #converting }
Array >> elementsForwardIdentityTo: otherArray copyHash: copyHash [
	"This primitive performs a bulk mutation, causing all pointers to the elements of the
	 receiver to be replaced by pointers to the corresponding elements of otherArray.
	 If copyHash is true, the identityHashes remain with the pointers rather than with the
	 objects so that the objects in the receiver should still be properly indexed in any
	 existing hashed structures after the mutation.  If copyHash is false, then the hashes
	 of the objects in otherArray remain unchanged.  If you know what you're doing this
	 may indeed be what you want."
	<primitive: 249 error: ec>
	self primitiveFailed
]

{ #category : #'literal testing' }
Array >> hasLiteral: literal [
	"Answer true if literal is identical to any literal in this array, even
	if imbedded in further array structure. This method is only intended
	for private use by CompiledMethod hasLiteralSymbol:"

	| lit |
	1 to: self size do:
		[:index |
		((lit := self at: index) literalEqual: literal) ifTrue: [^true].
		(Array == lit class and: [lit hasLiteral: literal]) ifTrue: [^true]].
	^false
]

{ #category : #'literal testing' }
Array >> hasLiteralSuchThat: testBlock [
	"Answer true if testBlock returns true for any literal in this array, even if imbedded in 	further Arrays or CompiledMethods.  This method is only intended for private use by 	CompiledMethod 	hasLiteralSuchThat:"
	| lit |
	1 to: self size do: [:index |
		(testBlock value: (lit := self at: index)) ifTrue: [^ true].
		(lit hasLiteralSuchThat: testBlock) ifTrue: [^ true]].
	^ false
]

{ #category : #testing }
Array >> isArray [
	^true
]

{ #category : #testing }
Array >> isLiteral [

	^ self class == Array and: [ self isLiteral: {  } ]
]

{ #category : #testing }
Array >> isLiteral: alreadyVisited [

	"Do not call super! This logic ends with an infinite recursion"

	(alreadyVisited identityIncludes: self) ifTrue: [ "Recursive structure cannot be literals"
		^ false ].

	^ self allSatisfy: [ :each | each isLiteral: alreadyVisited , { self } ]
]

{ #category : #'self evaluating' }
Array >> isSelfEvaluating [

	^ self class == Array and: [ self isSelfEvaluating: { } ]
]

{ #category : #'self evaluating' }
Array >> isSelfEvaluating: alreadyVisited [

	"Do not call super! This logic ends with an infinite recursion"

	(alreadyVisited identityIncludes: self) ifTrue: [ "Recursive structure cannot be a self evaluated"
		^ false ].

	^ self allSatisfy: [ :each | each isSelfEvaluating: alreadyVisited , { self } ]
]

{ #category : #comparing }
Array >> literalEqual: other [

	self class == other class ifFalse: [^ false].
	self size = other size ifFalse: [^ false].
	self with: other do: [:e1 :e2 |
		(e1 literalEqual: e2) ifFalse: [^ false]].
	^ true
]

{ #category : #'self evaluating' }
Array >> printAsLiteralFormOn: aStream [
	aStream nextPut: $#.
	self printElementsOn: aStream
]

{ #category : #'self evaluating' }
Array >> printAsSelfEvaluatingFormOn: aStream [

	aStream nextPut: ${.
	self do: [:el | el printOn: aStream] separatedBy: [ aStream nextPutAll: '. '].
	aStream nextPut: $}
]

{ #category : #printing }
Array >> printOn: aStream [
	self shouldBePrintedAsLiteral ifTrue: [self printAsLiteralFormOn: aStream. ^ self].
	self isSelfEvaluating ifTrue: [self printAsSelfEvaluatingFormOn: aStream. ^ self].

	super printOn: aStream
]

{ #category : #enumerating }
Array >> recursiveDo: aBlock [

	"execute the block for my content like do, but then recurse into nested arrays"

	self do: [ :each |
		aBlock value: each.
		each isArray ifTrue: [ each recursiveDo: aBlock ] ]
]

{ #category : #'literal testing' }
Array >> refersToLiteral: literal [
	^ (self literalEqual: literal)
		or: [ self hasLiteral: literal ]
]

{ #category : #accessing }
Array >> replaceFrom: start to: stop with: replacement startingAt: repStart [
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."
	<primitive: 105>
	super replaceFrom: start to: stop with: replacement startingAt: repStart
]

{ #category : #'literal testing' }
Array >> shouldBePrintedAsLiteral [

	^ self class == Array and: [ self shouldBePrintedAsLiteral: {  } ]
]

{ #category : #'literal testing' }
Array >> shouldBePrintedAsLiteral: alreadyVisited [

	"Do not call super! This logic ends with an infinite recursion"

	(alreadyVisited identityIncludes: self) ifTrue: [ "Recursive structure cannot be a literal"
		^ false ].

	^ self allSatisfy: [ :each | each shouldBePrintedAsLiteral: alreadyVisited , { self } ]
]

{ #category : #printing }
Array >> storeOn: aStream [

	"Use the literal form if possible."

	self shouldBePrintedAsLiteral
		ifTrue: [
			aStream
				nextPut: $#;
				nextPut: $(.
			self
				do: [ :element | element storeOn: aStream ]
				separatedBy: [ aStream space ].
			aStream nextPut: $) ]
		ifFalse: [ super storeOn: aStream ]
]
