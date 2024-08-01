"
I am an abstract superclass for collections that have a well-defined order associated with their elements. Thus each element is externally-named by integers referred to as indices.
"
Class {
	#name : 'SequenceableCollection',
	#superclass : 'Collection',
	#category : 'Collections-Sequenceable',
	#package : 'Collections-Sequenceable'
}

{ #category : 'testing' }
SequenceableCollection class >> isAbstract [

	^self name = #SequenceableCollection
]

{ #category : 'instance creation' }
SequenceableCollection class >> ofSize: n [
	"Create a new collection of size n with nil as its elements.
	This method exists because OrderedCollection new: n creates an
	empty collection,  not one of size n."
	^ self new: n
]

{ #category : 'stream creation' }
SequenceableCollection class >> streamContents: blockWithArg [
	"Build an instance of the receiver by writing elements to a stream.
	More specifically: blockWithArg will be given a WriteStream on an instance of the receiver.
	Inside blockWithArg you write elements to the stream to build up the collection.
	At the end, the contents of the stream up to that point will be returned.
	Note that the underlying collection grows as needed."

	"(Array streamContents: [ :out | out nextPut: 1; nextPutAll: #(2 3 4); nextPut: 5 ]) >>> #(1 2 3 4 5)"

	^ self new: 100 streamContents: blockWithArg
]

{ #category : 'accessing' }
SequenceableCollection class >> streamSpecies [
	"I return the class that is used for streaming. If override consider overriding #new:streamContents:"
	^ self
]

{ #category : 'copying' }
SequenceableCollection >> , otherCollection [
	"Concatenate two Strings or Collections."
	"#(2 4 6 8) , #(who do we appreciate) >>> #(2 4 6 8 who do we appreciate)"
	"((2989 storeStringBase: 16) copyFrom: 4 to: 6) , ' boy!' >>> 'BAD boy!'"

	^ self copyReplaceFrom: self size + 1
		  to: self size
		  with: otherCollection
]

{ #category : 'comparing' }
SequenceableCollection >> = otherCollection [
	"Answer true if the receiver is equivalent to the otherCollection.
	First test for identity, then rule out different species and sizes of
	collections. As a last resort, examine each element of the receiver
	and the otherCollection."

	self == otherCollection ifTrue: [^ true].
	self species == otherCollection species ifFalse: [^ false].
	^ self hasEqualElements: otherCollection
]

{ #category : 'converting' }
SequenceableCollection >> @ aCollection [
	^ self with: aCollection collect: [:a :b | a @ b]
]

{ #category : 'accessing' }
SequenceableCollection >> after: target [
	"Answer the element after target.  Raise an error if target is not
	in the receiver, or if there are no elements after it."
	"(#(a b c d) after: #b) >>> #c"

	^ self after: target ifAbsent: [self errorNotFound: target]
]

{ #category : 'accessing' }
SequenceableCollection >> after: target ifAbsent: exceptionBlock [
	"Answer the element after target.  Answer the result of evaluation
	the exceptionBlock if target is not in the receiver, or if there are
	no elements after it."
	"(#(a b c d) after: #b ifAbsent: #z) >>> #c"
	"(#(a b c d) after: #x ifAbsent: #z) >>> #z"

	| index |
	index := self indexOf: target.
	^ (index = 0 or: [index = self size])
		ifTrue: [exceptionBlock value]
		ifFalse: [self at: index + 1]
]

{ #category : 'accessing' }
SequenceableCollection >> allButFirst [
	"Answer a copy of the receiver containing all but the first
	element. Raise an error if there are not enough elements."
	"#(a b c) allButFirst >>> #(b c)"

	^ self allButFirst: 1
]

{ #category : 'accessing' }
SequenceableCollection >> allButFirst: n [
	"Answer a copy of the receiver containing all but the first n
	elements. Raise an error if there are not enough elements."
	"(#(a b c d) allButFirst: 2) >>> #(c d)"
	"(#(a b c d) allButFirst: 3) >>> #(d)"

	^ self copyFrom: n + 1 to: self size
]

{ #category : 'enumerating' }
SequenceableCollection >> allButFirstDo: aBlock [

	"Executes aBlock on each of the receiver's elements except for the first one"

	"(Array streamContents: [:stream | #(1 2 3) allButFirstDo: [:each | stream nextPut: (each + 10)]]) >>> #(12 13)"

	2 to: self size do:
		[:index | aBlock value: (self at: index)]
]

{ #category : 'accessing' }
SequenceableCollection >> allButLast [
	"Answer a copy of the receiver containing all but the last
	element. Raise an error if there are not enough elements."
	"#(a b c d) allButLast >>> #(a b c)"

	^ self allButLast: 1
]

{ #category : 'accessing' }
SequenceableCollection >> allButLast: n [
	"Answer a copy of the receiver containing all but the last n
	elements. Raise an error if there are not enough elements."
	"(#(a b c d) allButLast: 2) >>> #(a b)"
	"(#(a b c d) allButLast: 3) >>> #(a)"

	^ self copyFrom: 1 to: self size - n
]

{ #category : 'enumerating' }
SequenceableCollection >> allButLastDo: block [

	"Executes aBlock on each of the receiver's elements except for the last one"

	"(Array streamContents: [:stream | #(1 2 3) allButLastDo: [:each | stream nextPut: (each + 10)]]) >>> #(11 12)"

	1 to: self size - 1 do:
		[:index | block value: (self at: index)]
]

{ #category : 'enumerating' }
SequenceableCollection >> allPairsDo: aBinaryBlock [
	"Iterate over all the pairs of the receiver and apply the binaryBlock
	to each pair. "

	"(Array streamContents: [ :stream | #(0 1 2 3) allPairsDo: [ :first :second | stream nextPut: { first . second } ] ]) >>> #(#(0 0) #(0 1) #(0 2) #(0 3) #(1 0) #(1 1) #(1 2) #(1 3) #(2 0) #(2 1) #(2 2) #(2 3) #(3 0) #(3 1) #(3 2) #(3 3))"

	self do: [ :first | self do: [ :second | aBinaryBlock value: first value: second ] ]
]

{ #category : 'accessing' }
SequenceableCollection >> anyOne [
	"Answer a representative sample of the receiver. It raises an error when the collection is empty. This method can be helpful when needing to preinfer the nature of the contents of semi-homogeneous collections."
	"#(1 2 3) anyOne >>> 1"
	"
	([#() anyOne] on: SubscriptOutOfBounds do: [ :ex | 'whatever' ]) >>> 'whatever'
	"
	^ self first
]

{ #category : 'splitjoin' }
SequenceableCollection >> appendTo: aCollection [
	"double dispatch for join:"
	^ aCollection addAllLast: self
]

{ #category : 'converting' }
SequenceableCollection >> asArray [
	"Answer an Array whose elements are the elements of the receiver."

	^ Array withAll: self
]

{ #category : 'private' }
SequenceableCollection >> asDigitsAt: anInteger in: aCollection do: aBlock [
	self
		do: [ :each |
			aCollection at: anInteger put: each.
			anInteger = aCollection size
				ifTrue: [ aBlock value: aCollection ]
				ifFalse: [ self asDigitsAt: anInteger + 1 in: aCollection do: aBlock ] ]
]

{ #category : 'enumerating' }
SequenceableCollection >> asDigitsToPower: anInteger do: aBlock [
	"Repeatedly value aBlock with a single Array.  Adjust the collection
	so that aBlock is presented all (self size raisedTo: anInteger) possible
	combinations of the receiver's elements taken as digits of an anInteger long number."

	"(Array streamContents: [:stream | (0 to: 1) asDigitsToPower: 4 do: [:each | stream nextPut: each copy]]) >>> #(#(0 0 0 0) #(0 0 0 1) #(0 0 1 0) #(0 0 1 1) #(0 1 0 0) #(0 1 0 1) #(0 1 1 0) #(0 1 1 1) #(1 0 0 0) #(1 0 0 1) #(1 0 1 0) #(1 0 1 1) #(1 1 0 0) #(1 1 0 1) #(1 1 1 0) #(1 1 1 1))"

	| aCollection |
	aCollection := Array new: anInteger.
	self asDigitsAt: 1 in: aCollection do: aBlock
]

{ #category : 'converting' }
SequenceableCollection >> asStringWithCr [
	"Convert to a string with returns between items.  Elements are
usually strings.
	 Useful for labels for PopUpMenus."
	| labelStream |
	labelStream := (String new: 200) writeStream.
	self do: [:each |
		each isString
			ifTrue: [labelStream nextPutAll: each; cr]
			ifFalse: [each printOn: labelStream. labelStream cr]].
	self size > 0 ifTrue: [labelStream skip: -1].
	^ labelStream contents
]

{ #category : 'accessing' }
SequenceableCollection >> at: index ifAbsent: exceptionBlock [
	"Answer the element at my position index. If I do not contain an element
	at index, answer the result of evaluating the argument, exceptionBlock."
	"(#(a b c d) at: 3 ifAbsent: #z) >>> #c"
	"(#(a b c d) at: 5 ifAbsent: #z) >>> #z"

	(index between: 1 and: self size) ifTrue: [^ self at: index].
	^ exceptionBlock value
]

{ #category : 'accessing' }
SequenceableCollection >> at: index incrementBy: value [
	"({1 .2 .3 .4} at: 3 incrementBy: 2) >>> 5"
	"({1 .2 .3 .4}  at: 2 incrementBy: 2) >>> 4"

	^self at: index put: (self at: index) + value
]

{ #category : 'accessing' }
SequenceableCollection >> atAll: indexArray [
	"Answer a new collection like the receiver which contains all elements
	of the receiver at the indices of indexArray."
	"(#('one' 'two' 'three' 'four') atAll: #(3 2 4)) >>> #('three' 'two' 'four')"

	| newCollection |
	newCollection := self species ofSize: indexArray size.
	1 to: indexArray size do:
		[:index |
		newCollection at: index put: (self at: (indexArray at: index))].
	^ newCollection
]

{ #category : 'accessing' }
SequenceableCollection >> atAll: aCollection put: anObject [
	"Put anObject at every index specified by the elements of aCollection."
	"({#x .#y .#z} atAll: #(1 3) put: #e; yourself) >>> #(e y e)"

	aCollection do: [:index | self at: index put: anObject].
	^ anObject
]

{ #category : 'accessing' }
SequenceableCollection >> atAll: indexArray putAll: valueArray [
	"Store the elements of valueArray into the slots
	of this collection selected by indexArray."
	"({#x .#y .#z} atAll: #(1 3) putAll: #(a e); yourself) >>> #(a y e)"

	indexArray with: valueArray do: [:index :value | self at: index put: value].
	^ valueArray
]

{ #category : 'accessing' }
SequenceableCollection >> atAllPut: anObject [
	"Put anObject at every one of the receiver's indices."
	"({#x .#y. #z} atAllPut: #a) >>> #(a a a)"

	| size |
	(size := self size) > 26 "first method faster from 27 accesses and on"
		ifTrue: [self from: 1 to: size put: anObject]
		ifFalse: [1 to: size do: [:index | self at: index put: anObject]]
]

{ #category : 'accessing' }
SequenceableCollection >> atLast: indexFromEnd [
	"Return element at indexFromEnd from the last position.
	 atLast: 1, returns the last element"
	"(#(x y z) atLast: 1) >>> #z"
	"(#(x y z) atLast: 2) >>> #y"

	^ self atLast: indexFromEnd ifAbsent: [self error: 'index out of range']
]

{ #category : 'accessing' }
SequenceableCollection >> atLast: indexFromEnd ifAbsent: block [
	"Return element at indexFromEnd from the last position.
	 atLast: 1 ifAbsent: [] returns the last element"
	"(#(x y z) atLast: 1 ifAbsent: #a) >>> #z"
	"(#(x y z) atLast: 4 ifAbsent: #a) >>> #a"

	^ self at: self size + 1 - indexFromEnd ifAbsent: block
]

{ #category : 'accessing' }
SequenceableCollection >> atLast: indexFromEnd put: obj [
	"Set the element at indexFromEnd from the last position.
	 atLast: 1 put: obj, sets the last element"
	"({#x .#y .#z} atLast: 2 put: #e; yourself) >>> #(x e z)"

	^ self at: self size + 1 - indexFromEnd put: obj
]

{ #category : 'accessing' }
SequenceableCollection >> atPin: index [
	"Return the index'th element of me if possible.
	Return the first or last element if index is out of bounds."
	"(#(w x y z) atPin: 2) >>> #x"
	"(#(w x y z) atPin: 4) >>> #z"
	"(#(w x y z) atPin: 8) >>> #z"

	index < 1 ifTrue: [^ self first].
	index > self size ifTrue: [^ self last].
	^ self at: index
]

{ #category : 'accessing' }
SequenceableCollection >> atWrap: index [

    "Answer the index'th element of the receiver.  If index is out of bounds,
    let it wrap around from the end to the beginning until it is in bounds."
    "(#(11 22 33) asOrderedCollection atWrap: 2) >>> 22"
    "(#(11 22 33) asOrderedCollection atWrap: 4) >>> 11"
    "(#(11 22 33) asOrderedCollection atWrap: 5) >>> 22"

    ^ self at: index - 1 \\ self size + 1
]

{ #category : 'accessing' }
SequenceableCollection >> atWrap: index put: value [
	"Store value into the index'th element of the receiver.  If index is out
	of bounds, let it wrap around from the end to the beginning until it
	is in bounds. Answer value."
	"(#(11 22 33) asOrderedCollection atWrap: 2 put: 0; yourself) >>> #(11 0 33) asOrderedCollection"
   "(#(11 22 33) asOrderedCollection atWrap: 4 put: 0; yourself) >>> #(0 22 33) asOrderedCollection"
   "(#(11 22 33) asOrderedCollection atWrap: 6 put: 0; yourself) >>> #(11 22 0) asOrderedCollection"

	^ self at: index  - 1 \\ self size + 1 put: value
]

{ #category : 'accessing' }
SequenceableCollection >> before: target [
	"Answer the receiver's element immediately before target. Raise an
	error if target is not an element of the receiver, or if there are no
	elements before it (i.e. it is the first element)."
	"(#(11 22 33) before: 22) >>> 11"
	"(#(11 22 33) before: 33) >>> 22"

	^ self before: target ifAbsent: [self errorNotFound: target]
]

{ #category : 'accessing' }
SequenceableCollection >> before: target ifAbsent: exceptionBlock [
	"Answer the receiver's element immediately before target. Answer
	the result of evaluating the exceptionBlock if target is not an element
	of the receiver, or if there are no elements before it."
	"(#(11 22 33) before: 22 ifAbsent: 55) >>> 11"
	"(#(11 22 33) before: 44 ifAbsent: 55) >>> 55"

	| index |
	index := self indexOf: target.
	^ (index = 0 or: [index = 1])
		ifTrue: [exceptionBlock value]
		ifFalse: [self at: index - 1]
]

{ #category : 'testing' }
SequenceableCollection >> beginsWith: aSequenceableCollection [
	"Answer true if the receiver starts with the argument collection"

	"(#(1 2 3 4 5) beginsWith: #()) >>> true"
	"(#(1 2 3) beginsWith: #(1 2 3 4 5)) >>> false"
	"(#(1 2 3 4 5) beginsWith: #(0 1 2)) >>> false"
	"(#(1 2 3 4 5) beginsWith: #(1 2 3)) >>> true"

	aSequenceableCollection ifEmpty: [ ^true ].
	self size < aSequenceableCollection size ifTrue: [^false].
	aSequenceableCollection withIndexDo: [:each :index | (self at: index) ~= each ifTrue: [^false]].
	^true
]

{ #category : 'testing' }
SequenceableCollection >> beginsWithAnyOf: aCollection [
	"Return true if the receiver starts with any of the elements in aCollection."
	^aCollection anySatisfy:[:prefix| self beginsWith: prefix]
]

{ #category : 'enumerating' }
SequenceableCollection >> bind: aBlock [

	"Extract items from the receiver and use them as arguments of aBlock. Signal an error
	(ArgumentsCountMismatch) when the arguments count of aBlock does not match the receiver size."

	"(#(1 2 3) bind: [ :first :second :third | first + second + third ]) >>> 6"

	^ aBlock valueWithArguments: self
]

{ #category : 'enumerating' }
SequenceableCollection >> collect: aBlock [
	"Evaluate aBlock with each of the receiver's elements as the argument.
	Collect the resulting values into a collection like the receiver. Answer
	the new collection."

	"(#(1 2 3) collect: [:each | each  + 10]) >>> #(11 12 13) "

	| newCollection |
	newCollection := self species new: self size.
	1 to: self size do:
		[:index |
		newCollection at: index put: (aBlock value: (self at: index))].
	^ newCollection
]

{ #category : 'enumerating' }
SequenceableCollection >> collect: aBlock from: firstIndex to: lastIndex [
	"Refer to the comment in Collection|collect:."

	"(#(1 2 3 4) collect: [:each | each  + 10] from: 2 to: 3) >>> #(12 13) "

	| size result j |
	size := lastIndex - firstIndex + 1.
	result := self species new: size.
	j := firstIndex.
	1 to: size do: [:i | result at: i put: (aBlock value: (self at: j)). j := j + 1].
	^ result
]

{ #category : 'enumerating' }
SequenceableCollection >> collect: collectBlock thenReject: rejectBlock [
	"Optimized version of Collection>>#thenReject:"

	"(#(1 2 3) collect: [:each | each + 10 ] thenReject: [:each | each even]) >>> #(11 13)"

	| each |

	^  self class new: self size streamContents: [ :stream |
		1 to: self size do: [:index |
			each := collectBlock value: (self at: index).
			(rejectBlock value: each)
				ifFalse: [ stream nextPut: each ]]]
]

{ #category : 'enumerating' }
SequenceableCollection >> collect: collectBlock thenSelect: selectBlock [
	"Optimized version of Collection>>#collect:thenSelect:"

	"(#(1 2 3) collect: [:each | each + 10 ] thenSelect: [:each | each even]) >>> #(12)"

	| each |

	^  self class new: self size streamContents: [ :stream |
		1 to: self size do: [:index |
			each := collectBlock value: (self at: index).
			(selectBlock value: each)
				ifTrue: [ stream nextPut: each ]]]
]

{ #category : 'enumerating' }
SequenceableCollection >> collectWithIndex: elementAndIndexBlock [
	"Use the new version with consistent naming"
	^ self withIndexCollect: elementAndIndexBlock
]

{ #category : 'enumerating' }
SequenceableCollection >> combinations [
	"Return all the combinations of elements of the receiver. Note that combinations does not include the empty element contrary to the mathematical definition of combinations."

	"#(1 2 3 4) combinations >>> #(#(1) #(2) #(3) #(4) #(1 2) #(1 3) #(1 4) #(2 3) #(2 4) #(3 4) #(1 2 3) #(1 2 4) #(1 3 4) #(2 3 4) #(1 2 3 4))"

	^ Array
		streamContents: [ :stream |
			1 to: self size do:
				[ :take | self combinations: take atATimeDo: [ :combination | stream nextPut: combination copy ] ] ]
]

{ #category : 'enumerating' }
SequenceableCollection >> combinations: aNumber [

	"Take the items in the receiver, aNumber at a time, return all the combinations"

	^ Array streamContents: [ :stream |
		  self
			  combinations: aNumber
			  atATimeDo: [ :combination | stream nextPut: combination copy ] ]
]

{ #category : 'enumerating' }
SequenceableCollection >> combinations: kk atATimeDo: aBlock [
	"Take the items in the receiver, kk at a time, and evaluate the block for each combination.  Hand in an array of elements of self as the block argument.  Each combination only occurs once, and order of the elements does not matter.  There are (self size take: kk) combinations."

	"(Array streamContents: [:stream | 'abcde' combinations: 3 atATimeDo: [:each | stream nextPut: each copy]]) >>> #(#($a $b $c) #($a $b $d) #($a $b $e) #($a $c $d) #($a $c $e) #($a $d $e) #($b $c $d) #($b $c $e) #($b $d $e) #($c $d $e))"

	| aCollection |
	aCollection := Array new: kk.
	self
		combinationsAt: 1
		in: aCollection
		after: 0
		do: aBlock
]

{ #category : 'private' }
SequenceableCollection >> combinationsAt: jj in: aCollection after: nn do: aBlock [
	"Choose k of N items and put in aCollection.  jj-1 already chosen.  Indexes of items are in numerical order, to avoid the same combo being used twice.  In this slot, we are allowed to use items in self indexed by nn+1 to self size.  nn is the index used for position jj-1."

	nn + 1 to: self size do: [ :index |
		aCollection at: jj put: (self at: index).
		jj = aCollection size
			ifTrue: [ aBlock value: aCollection ]
			ifFalse: [
				self
					combinationsAt: jj + 1
					in: aCollection
					after: index
					do: aBlock ] ]
]

{ #category : 'copying' }
SequenceableCollection >> copyAfter: anElement [
	"Answer a copy of the receiver from after the first occurrence
	of anElement up to the end. If no such element exists, answer
	an empty copy."

	^ self allButFirst: (self indexOf: anElement ifAbsent: [^ self copyEmpty])
]

{ #category : 'copying' }
SequenceableCollection >> copyAfterLast: anElement [
	"Answer a copy of the receiver from after the last occurrence
	of anElement up to the end. If no such element exists, answer
	an empty copy."

	^ self allButFirst: (self lastIndexOf: anElement ifAbsent: [^ self copyEmpty])
]

{ #category : 'copying' }
SequenceableCollection >> copyEmpty [
	^ self species new: 0
]

{ #category : 'copying' }
SequenceableCollection >> copyFrom: start to: stop [
	"Answer a copy of a subset of the receiver, starting from element at
	index start until element at index stop."

	| newSize |
	newSize := stop - start + 1.
	^(self species new: newSize)
		replaceFrom: 1
		to: newSize
		with: self
		startingAt: start
]

{ #category : 'copying' }
SequenceableCollection >> copyReplaceAll: oldSubCollection with: newCollection [
	"Answer a copy of the receiver in which all occurrences of
	oldSubCollection have been replaced by newCollection "

	| aString startSearch currentIndex endIndex |

	aString := self.
	startSearch := 1.
	[(currentIndex := aString indexOfSubCollection: oldSubCollection startingAt: startSearch) > 0]
		whileTrue: [
			endIndex := currentIndex + oldSubCollection size - 1.
			aString := aString
					copyReplaceFrom: currentIndex
					to: endIndex
					with: newCollection.
				startSearch := currentIndex + newCollection size].

	^ aString
]

{ #category : 'copying' }
SequenceableCollection >> copyReplaceFrom: start to: stop with: replacementCollection [
	"Answer a copy of the receiver satisfying the following conditions: If
	stop is less than start, then this is an insertion; stop should be exactly
	start-1, start = 1 means insert before the first character, start = size+1
	means append after last character. Otherwise, this is a replacement; start
	and stop have to be within the receiver's bounds."

	| newSequenceableCollection newSize endReplacement |
	newSize := self size - (stop - start + 1) + replacementCollection size.
	endReplacement := start - 1 + replacementCollection size.
	newSequenceableCollection := self species new: newSize.
	start > 1 ifTrue:[
		newSequenceableCollection
			replaceFrom: 1
			to: start - 1
			with: self
			startingAt: 1].
	start <= endReplacement ifTrue:[
		newSequenceableCollection
			replaceFrom: start
			to: endReplacement
			with: replacementCollection
			startingAt: 1].
	endReplacement < newSize ifTrue:[
		newSequenceableCollection
			replaceFrom: endReplacement + 1
			to: newSize
			with: self
			startingAt: stop + 1].
	^newSequenceableCollection
]

{ #category : 'copying' }
SequenceableCollection >> copyUpThrough: anElement [
    "Answer all elements up to and including anObject. If there
     is no such object, answer a copy of the receiver."

	^self first: (self indexOf: anElement ifAbsent: [^ self copy])
]

{ #category : 'copying' }
SequenceableCollection >> copyUpTo: anElement [
	"Answer all elements up to but not including anObject. If there
	is no such object, answer a copy of the receiver."

	^ self first: (self indexOf: anElement ifAbsent: [^ self copy]) - 1
]

{ #category : 'copying' }
SequenceableCollection >> copyUpToLast: anElement [
	"Answer a copy of the receiver from index 1 to the last occurrence of
	anElement, not including anElement."

	^ self first: (self lastIndexOf: anElement ifAbsent: [^ self copy]) - 1
]

{ #category : 'copying' }
SequenceableCollection >> copyWith: newElement [
	"Answer a copy of the receiver that is 1 bigger than the receiver and has
	newElement at the last element."

	| newIC |
	newIC := self species new: self size + 1.
	newIC
		replaceFrom: 1
		to: self size
		with: self
		startingAt: 1.
	newIC at: newIC size put: newElement.
	^newIC
]

{ #category : 'copying' }
SequenceableCollection >> copyWithFirst: newElement [
	"Answer a copy of the receiver that is 1 bigger than the receiver with newElement as the first element."

	| newIC |
	newIC := self species ofSize: self size + 1.
	newIC
		replaceFrom: 2
		to: self size + 1
		with: self
		startingAt: 1.
	newIC at: 1 put: newElement.
	^ newIC
]

{ #category : 'copying' }
SequenceableCollection >> copyWithoutFirst [
	"Deprecatd. Return a copy of the receiver which doesn't include
	the first element."

	^ self allButFirst
]

{ #category : 'copying' }
SequenceableCollection >> copyWithoutIndex: index [
	"Return a copy containing all elements except the index-th."

	| copy |
	copy := self species ofSize: self size - 1.
	copy replaceFrom: 1 to: index-1 with: self startingAt: 1.
	copy replaceFrom: index to: copy size with: self startingAt: index+1.
	^ copy
]

{ #category : 'enumerating' }
SequenceableCollection >> detectIndex: aBlock [

	"Return index of first element that satisfies aBlock.
	If no matching element is found, raise an error."

	"(#(1 5 10) detectIndex: [ :each | each > 3 ]) >>> 2"

	^ self detectIndex: aBlock ifNone: [ self errorNotFound: aBlock ]
]

{ #category : 'enumerating' }
SequenceableCollection >> detectIndex: aBlock ifNone: exceptionBlock [

	"Return index of first element that satisfies aBlock.
	If no matching element is found, evaluate exceptionBlock."

	"(#(1 5 10) detectIndex: [ :each | each > 3 ] ifNone: ['Not found']) >>> 2"
	"(#(1 5 10) detectIndex: [ :each | each > 15 ] ifNone: ['Not found']) >>> 'Not found'"

	self doWithIndex: [:each :index | (aBlock value: each) ifTrue: [^ index]].
	^ exceptionBlock value
]

{ #category : 'enumerating' }
SequenceableCollection >> difference: aCollection [
	"Answer the difference of two sequences preserving order and collection type."
	^ self reject: [:each | aCollection includes: each]
]

{ #category : 'enumerating' }
SequenceableCollection >> do: aBlock [
	"Refer to the comment in Collection|do:."
	1 to: self size do:
		[:index | aBlock value: (self at: index)]
]

{ #category : 'enumerating' }
SequenceableCollection >> do: elementBlock separatedBy: separatorBlock [
	"Evaluate the elementBlock for all elements in the receiver,
	and evaluate the separatorBlock between."

	"(Array streamContents: ([:stream | #(1 2 3) do: [:each | stream nextPut: (each + 10)] separatedBy: [ stream nextPut: 0 ]])) >>> #(11 0 12 0 13)"

	1 to: self size do:
		[:index |
		index = 1 ifFalse: [separatorBlock value].
		elementBlock value: (self at: index)]
]

{ #category : 'enumerating' }
SequenceableCollection >> do: aBlock without: anItem [
	"Enumerate all elements in the receiver.
	Execute aBlock for those elements that are not equal to the given item"
	"Refer to the comment in Collection|do:."

	"(Array streamContents: ([:stream | #(1 2 3) do: [:each | stream nextPut: (each + 10)] without: 2])) >>> #(11 13)"

	1 to: self size do:
		[:index | anItem = (self at: index) ifFalse:[aBlock value: (self at: index)]]
]

{ #category : 'accessing' }
SequenceableCollection >> eighth [
	"Answer the eighth element of the receiver.
	Raise an error if there are not enough elements."
	"#(a b c d e f g h ) eighth  >>> #h"

	^ self at: 8
]

{ #category : 'testing' }
SequenceableCollection >> endsWith: aSequenceableCollection [
	"Answer true if the receiver ends with the argument collection"

	| start |
	aSequenceableCollection ifEmpty: [ ^true ].
	(self size < aSequenceableCollection size) ifTrue: [^false].
	start := self size - aSequenceableCollection size.
	aSequenceableCollection withIndexDo: [:each :index | (self at: start + index) ~= each ifTrue: [^false]].
	^true
]

{ #category : 'testing' }
SequenceableCollection >> endsWithAnyOf: aCollection [
	"Return true if the receiver ends with any of the elements in aCollection."
	^aCollection anySatisfy:[:suffix| self endsWith: suffix]
]

{ #category : 'private' }
SequenceableCollection >> errorOutOfBounds [

	SubscriptOutOfBounds signal
]

{ #category : 'accessing' }
SequenceableCollection >> fifth [
	"Answer the fifth element of the receiver.
	Raise an error if there are not enough elements."
	"#(a b c d e f g h ) fifth >>> #e"

	^ self at: 5
]

{ #category : 'enumerating' }
SequenceableCollection >> findBinary: aBlock [
	"Search for an element in the receiver using binary search.
	The argument aBlock is a one-element block returning
		0 	- if the element is the one searched for
		<0	- if the search should continue in the first half
		>0	- if the search should continue in the second half
	If no matching element is found, raise an error."

	"(#(1 3 5 7 11 15 23) findBinary: [ :arg | 11 - arg ]) >>> 11 "

	^ self findBinary: aBlock do: [ :found | found ] ifNone: [ self errorNotFound: aBlock ]
]

{ #category : 'enumerating' }
SequenceableCollection >> findBinary: aBlock do: actionBlock ifNone: exceptionBlock [
	"Search for an element in the receiver using binary search.
	The argument aBlock is a one-element block returning
		0 	- if the element is the one searched for
		<0	- if the search should continue in the first half
		>0	- if the search should continue in the second half
	If found, evaluate actionBlock with the found element as argument
	If no matching element is found, evaluate exceptionBlock,
	with the 'bounding' elements or nil as arguments."

	"(#(1 3 5 7 11 15 23) findBinary: [ :arg | 11 - arg ] do: [ :found | found ] ifNone: [ :a :b | 'between: ', {a. b} printString ]) >>> 11"

	"(#(1 3 5 7 11 15 23) findBinary: [ :arg | 12 - arg ] do: [ :found | found ] ifNone: [ :a :b | 'between: ', {a. b} printString ]) >>> 'between: #(11 15)'"

	"(#(1 3 5 7 11 15 23) findBinary: [ :arg | 0.5 - arg ] do: [ :found | found ] ifNone: [ :a :b | 'between: ', {a. b} printString ]) >>> 'between: #(nil 1)'"

	"(#(1 3 5 7 11 15 23) findBinary: [ :arg | 25 - arg ] do: [ :found | found ] ifNone: [ :a :b | 'between: ',{a. b} printString ]) >>> 'between: #(23 nil)'"

	^ self
		findBinaryIndex: aBlock
		do: [ :foundIndex | actionBlock value: (self at: foundIndex) ]
		ifNone: [ :prevIndex :nextIndex |
			exceptionBlock
				cull:
					(prevIndex > 0
						ifTrue: [ self at: prevIndex ])
				cull:
					(nextIndex <= self size
						ifTrue: [ self at: nextIndex ]) ]
]

{ #category : 'enumerating' }
SequenceableCollection >> findBinary: aBlock ifNone: exceptionBlock [
	"Search for an element in the receiver using binary search.
	The argument aBlock is a one-element block returning
		0 	- if the element is the one searched for
		<0	- if the search should continue in the first half
		>0	- if the search should continue in the second half
	If no matching element is found, evaluate exceptionBlock,
	with the 'bounding' elements as optional arguments."

	"(#(1 3 5 7 11 15 23) findBinary: [ :arg | 24 - arg ] ifNone: ['Not found']) >>> 'Not found'"

	"(#(1 3 5 7 11 15 23) findBinary: [ :arg | 24 - arg ] ifNone: [:a :b | 'over ', a printString]) >>> 'over 23'"

	"(#(1 3 5 7 11 15 23) findBinary: [ :arg | 25 - arg ] ifNone: [ :a :b | 'between: ',{a. b} printString ]) >>> 'between: #(23 nil)'"

	^ self
		findBinary: aBlock
		do: [ :found | found ]
		ifNone: exceptionBlock
]

{ #category : 'enumerating' }
SequenceableCollection >> findBinaryIndex: aBlock [
	"Search for an element in the receiver using binary search.
	The argument aBlock is a one-element block returning
		0 	- if the element is the one searched for
		<0	- if the search should continue in the first half
		>0	- if the search should continue in the second half
	If no matching element is found, raise an error."

	"(#(1 3 5 7 11 15 23) findBinaryIndex: [ :arg | 11 - arg ]) >>> 5"

	^ self
		findBinaryIndex: aBlock
		do: [ :found | found ]
		ifNone: [ self errorNotFound: aBlock ]
]

{ #category : 'enumerating' }
SequenceableCollection >> findBinaryIndex: aBlock do: actionBlock ifNone: exceptionBlock [
	"Search for an element in the receiver using binary search.
	The argument aBlock is a one-element block returning
		0 	- if the element is the one searched for
		<0	- if the search should continue in the first half
		>0	- if the search should continue in the second half
	If found, evaluate actionBlock with the index as argument
	If no matching element is found, evaluate exceptionBlock,
	with the indexes of the 'bounding' elements as arguments.
	Warning: Might give invalid indexes, see examples below"

	"(#(1 3 5 7 11 15 23) findBinaryIndex: [ :arg | 11 - arg ] do: [ :found | found ] ifNone: [ :a :b | 'between: ', {a. b} printString ]) >>> 5"
	"(#(1 3 5 7 11 15 23) findBinaryIndex: [ :arg | 12 - arg ] do: [ :found | found ] ifNone: [ :a :b | 'between: ', {a. b} printString ]) >>> 'between: #(5 6)'"
	"(#(1 3 5 7 11 15 23) findBinaryIndex: [ :arg | 0.5 - arg ] do: [ :found | found ] ifNone: [ :a :b | 'between: ', {a. b} printString ]) >>> 'between: #(0 1)'"
	"(#(1 3 5 7 11 15 23) findBinaryIndex: [ :arg | 25 - arg ] do: [ :found | found ] ifNone: [ :a :b | 'between: ',{a. b} printString ]) >>> 'between: #(7 8)' "

	| index low high test |
	low := 1.
	high := self size.
	[ index := high + low // 2.
	  low > high ] whileFalse: [
		test := aBlock value: (self at: index).
		test = 0
			ifTrue: [ ^ actionBlock value: index ]
			ifFalse: [ test > 0
				ifTrue: [ low := index + 1 ]
				ifFalse: [ high := index - 1 ] ] ].
	^ exceptionBlock cull: high cull: low
]

{ #category : 'enumerating' }
SequenceableCollection >> findBinaryIndex: aBlock ifNone: exceptionBlock [
	"Search for an element in the receiver using binary search.
	The argument aBlock is a one-element block returning
		0 	- if the element is the one searched for
		<0	- if the search should continue in the first half
		>0	- if the search should continue in the second half
	If no matching element is found, evaluate exceptionBlock,
	with the 'bounding' elements as optional arguments."

	"(#(1 3 5 7 11 15 23) findBinaryIndex: [ :arg | 25 - arg ] ifNone: [ :a :b | 'between: ',{a. b} printString ]) >>> 'between: #(7 8)' "

	"(#(1 3 5 7 11 15 23) findBinaryIndex: [ :arg | 25 - arg ] ifNone: [ :a :b | 'over index: ', a printString ]) >>> 'over index: 7' "

	^ self
		findBinaryIndex: aBlock
		do: [ :found | found ]
		ifNone: exceptionBlock
]

{ #category : 'enumerating' }
SequenceableCollection >> findFirst: aBlock [
	"Return the index of my first element for which aBlock evaluates as true.
	If no matching element is found, return 0"

	"(#(1 5 10) findFirst: [ :each | each > 3 ]) >>> 2"
	"(#(1 5 10) findFirst: [ :each | each > 15 ]) >>> 0"

	| index |
	index := 0.
	[(index := index + 1) <= self size] whileTrue:
		[(aBlock value: (self at: index)) ifTrue: [^index]].
	^ 0
]

{ #category : 'enumerating' }
SequenceableCollection >> findLast: aBlock [
	"Return the index of my last element for which aBlock evaluates as true.
	If no matching element is found, return 0"

	"(#(10 20 30 40 50) findLast: [ :each | each > 10 ]) >>> 5"
	"(#(10 20 30 40 50) findLast: [ :each | each > 60 ]) >>> 0"

	| index |
	index := self size + 1.
	[(index := index - 1) >= 1] whileTrue:
		[(aBlock value: (self at: index)) ifTrue: [^index]].
	^ 0
]

{ #category : 'accessing' }
SequenceableCollection >> first [
	"Answer the first element of the receiver"
	"#(a b c d e f g h ) first >>> #a"

	^ self at: 1
]

{ #category : 'accessing' }
SequenceableCollection >> first: n [
	"Answer the first n elements of the receiver.
	Raise an error if there are not enough elements."
	"(#(a b c d e f g h ) first: 3) >>> #(a b c)"

	^ self copyFrom: 1 to: n
]

{ #category : 'enumerating' }
SequenceableCollection >> flatCollect: aBlock [
	"Evaluate aBlock for each of the receiver's elements and answer the
	list of all resulting values flatten one level. Assumes that aBlock returns some kind
	of collection for each element. optimized version for Sequencable Collection and subclasses
	implementing #writeStream"

	"(#( (2 -3) (4 -5) #(-6)) flatCollect: [ :e | e abs  ]) >>> #(2 3 4 5 6)"

	"(#( (2 -3) #((4 -5)) #(-6)) flatCollect: [ :e | e abs  ]) >>> #(2 3 #(4 5) 6)"

	self isEmpty
		ifTrue: [ ^ self copy ].
	^self species
		new: 0
		streamContents: [ :stream | self do: [ :each | stream nextPutAll: (aBlock value: each) ] ]
]

{ #category : 'copying' }
SequenceableCollection >> forceTo: length paddingStartWith: elem [
	"Force the length of the collection to length, padding
	the beginning of the result if necessary with elem.
	Note that this makes a copy."
	| newCollection padLen |
	newCollection := self species ofSize: length.
	padLen := length - self size max: 0.
	newCollection
		from: 1
		to: padLen
		put: elem.
	newCollection
		replaceFrom: padLen + 1
		to: ((padLen + self size) min: length)
		with: self
		startingAt:  1.
	^ newCollection
]

{ #category : 'copying' }
SequenceableCollection >> forceTo: length paddingWith: elem [
	"Force the length of the collection to length, padding
	if necessary with elem.  Note that this makes a copy."

	| newCollection |
	newCollection := self species new: length withAll: elem.
	newCollection replaceFrom: 1 to: (self size min: length) with: self startingAt: 1.
	^ newCollection
]

{ #category : 'accessing' }
SequenceableCollection >> fourth [
	"Answer the fourth element of the receiver.
	Raise an error if there are not enough elements."
	"#(a b c d e f g h ) fourth >>> #d"

	^ self at: 4
]

{ #category : 'enumerating' }
SequenceableCollection >> from: start to: stop do: aBlock [
	"Evaluate aBlock for all elements between start and stop (inclusive)."

	"(Array streamContents: [:stream | #(10 20 30 40) from: 2 to: 3 do: [:each | stream nextPut: each]]) >>> #(20 30)"

	start to: stop do: [:index | aBlock value: (self at: index)]
]

{ #category : 'accessing' }
SequenceableCollection >> from: startIndex to: endIndex put: anObject [
	"Put anObject in all indexes between startIndex
	and endIndex. Very fast. Faster than to:do: for
	more than 26 positions. Answer anObject"
	"({#a. #b. #c. #d. #e} from: 3 to: 4 put: #x; yourself) >>> #(a b x x e)"

	| written toWrite thisWrite |

	startIndex > endIndex ifTrue: [^self].
	self at: startIndex put: anObject.
	written := 1.
	toWrite := endIndex - startIndex + 1.
	[written < toWrite] whileTrue:
		[
			thisWrite := written min: toWrite - written.
			self
				replaceFrom: startIndex + written
				to: startIndex + written + thisWrite - 1
				with: self startingAt: startIndex.
			written := written + thisWrite
		].
	^anObject
]

{ #category : 'enumerating' }
SequenceableCollection >> groupByRuns: aBlock [
	"Answer a new collection of the same species as the receiver with elements being collections (of the receiver species) containing those elements of the receiver for which the given block consecutively evaluates to the same object."

	"(#(1 2 3 4 4 1 2 3 5 6 ) groupByRuns: [ :each | each = 4]) >>> #(#(1 2 3) #(4 4) #(1 2 3 5 6))"

	"(#(1 2 3 4 1 2 3 4 5 6 ) groupByRuns: [ :each | each = 4]) >>> #(#(1 2 3) #(4) #(1 2 3) #(4) #(5 6))"

	"((1 to: 12) groupByRuns: [ :each | (each \\ 3) = 0]) >>> #(#(1 2) #(3) #(4 5) #(6) #(7 8) #(9) #(10 11) #(12))"

	| str eStr r |
	str := Array new writeStream.
	r := nil.
	eStr := Array new writeStream.
	self
		do: [ :e |
			| t |
			(t := aBlock value: e) = r
				ifFalse: [ r := t.
					eStr isEmpty
						ifFalse: [ str nextPut: (eStr contents as: self species).
							eStr reset ] ].
			eStr nextPut: e ].
	eStr isEmpty
		ifFalse: [ str nextPut: (eStr contents as: self species) ].
	^ str contents as: self species
]

{ #category : 'enumerating' }
SequenceableCollection >> groupsOf: n atATimeCollect: aBlock [
	"Evaluate aBlock with my elements taken n at a time. Ignore any leftovers at the end.
	Allows use of a flattened array for things that naturally group into groups of n.
	If aBlock has a single argument, pass it an array of n items, otherwise, pass the items as separate arguments. See also pairsDo:"

	"(#(16 17 17 16 18 17 18   19 19 19 18 19 19 20   19 20 19 20 20 20 19   20) groupsOf: 7 atATimeCollect: [ :x | x ]) >>> #(#(16 17 17 16 18 17 18) #(19 19 19 18 19 19 20) #(19 20 19 20 20 20 19))"

	"(#(1 1 1 10 10 10 100  100 100) groupsOf: 3 atATimeCollect: [ :x :y :z | x + y + z]) >>> #(3 30 300)"

	"(#(1 1 1 10 10 10 100  100 100) groupsOf: 3 atATimeCollect: [ :x | x ]) >>> #(#(1 1 1) #(10 10 10) #(100 100 100))"

	| passArray |
	passArray := aBlock numArgs <= 1.
	^ (n to: self size by: n)
		collect: [ :index |
			| args |
			args := (self copyFrom: index - n + 1 to: index) asArray.
			passArray
				ifTrue: [ aBlock value: args ]
				ifFalse: [ aBlock valueWithArguments: args ] ]
]

{ #category : 'enumerating' }
SequenceableCollection >> groupsOf: n atATimeDo: aBlock [
	"Evaluate aBlock with my elements taken n at a time. Ignore any leftovers at the end.
	Allows use of a flattened
	array for things that naturally group into groups of n.
	If aBlock has a single argument, pass it an array of n items,
	otherwise, pass the items as separate arguments.
	See also pairsDo:"

	"(Array streamContents: [:stream | #(10 20 30 40 50) groupsOf: 2 atATimeDo:  [:first :second | stream nextPut: (first + second)]]) >>> #(30 70)"

	| passArray |
	passArray := (aBlock numArgs = 1).
	n
		to: self size
		by: n
		do: [:index | | args |
			args := (self copyFrom: index - n + 1 to: index) asArray.
			passArray ifTrue: [ aBlock value: args ]
				ifFalse: [ aBlock valueWithArguments: args ]]
]

{ #category : 'copying' }
SequenceableCollection >> grownBy: length [
	"Answer a copy of receiver collection with size grown by length"

	| newCollection size |
	size := self size.
	newCollection := self species ofSize: size + length.
	newCollection replaceFrom: 1 to: size with: self startingAt: 1.
	^ newCollection
]

{ #category : 'comparing' }
SequenceableCollection >> hasEqualElements: otherCollection [
	"Answer whether the receiver's size is the same as otherCollection's
	size, and each of the receiver's elements equal the corresponding
	element of otherCollection.
	This should probably replace the current definition of #= ."

	| size |
	(otherCollection isKindOf: SequenceableCollection) ifFalse: [^ false].
	(size := self size) = otherCollection size ifFalse: [^ false].
	1 to: size do:
		[:index |
		(self at: index) = (otherCollection at: index) ifFalse: [^ false]].
	^ true
]

{ #category : 'comparing' }
SequenceableCollection >> hash [
	| hash |

	hash := self species hash.
	1 to: self size do: [:i | hash := (hash + (self at: i) hash) hashMultiply].
	^hash
]

{ #category : 'accessing' }
SequenceableCollection >> identityIndexOf: anElement [
	"Answer the index of anElement within the receiver. If the receiver does
	not contain anElement, answer 0."
	"(#(a b c d e) identityIndexOf: #c) >>> 3"
	"(#(a b c d e) identityIndexOf: #x) >>> 0"

	^self identityIndexOf: anElement ifAbsent: [0]
]

{ #category : 'accessing' }
SequenceableCollection >> identityIndexOf: anElement ifAbsent: exceptionBlock [
	"Answer the index of anElement within the receiver. If the receiver does
	not contain anElement, answer the result of evaluating the argument,
	exceptionBlock."
	"(#(a b c d e) identityIndexOf: #c ifAbsent: 7) >>> 3"
	"(#(a b c d e) identityIndexOf: #x ifAbsent: 7) >>> 7"

	1 to: self size do:
		[:i | (self at: i) == anElement ifTrue: [^ i]].
	^ exceptionBlock value
]

{ #category : 'accessing' }
SequenceableCollection >> ifCutOn: isSplitterBlock doWithCutAndUncuts: aTwoArgBlock [
	"Applies aTwoArgBlock (with current splitter objects and previous unsplit objects) to the receiver.
	An optimised version could work with indexes to avoid creating intermediate collections."

	| uncuts cut current |
	uncuts := OrderedCollection new.
	1 to: self size do: [ :i |
		current := self at: i.
		cut := isSplitterBlock value: current.
		cut
			ifFalse: [ uncuts addLast: current ]
			ifTrue: [
					aTwoArgBlock value: current value: uncuts.
					uncuts := OrderedCollection new ]]
]

{ #category : 'accessing' }
SequenceableCollection >> ifCutOn: isSplitterBlock doWithCutAndUncuts: aTwoArgBlock finally: aBlock [
	"Applies aTwoArgBlock (with current splitter objects and previous unsplit objects) to the receiver. When uncuts are left executes aBlock with them.
	An optimised version could work with indexes to avoid creating intermediate collections."

	| uncuts cut current |
	uncuts := OrderedCollection new.
	1 to: self size do: [ :i |
		current := self at: i.
		cut := isSplitterBlock value: current.
		cut
			ifFalse: [ uncuts addLast: current ]
			ifTrue: [
					aTwoArgBlock value: current value: uncuts.
					uncuts := OrderedCollection new ]].
	uncuts isEmpty
		ifFalse: [ aBlock value: uncuts ]
]

{ #category : 'testing' }
SequenceableCollection >> includes: anObject [
	"Answer whether anObject is one of the receiver's elements."

	^ (self indexOf: anObject) ~= 0
]

{ #category : 'accessing' }
SequenceableCollection >> indexOf: anElement [
	"Answer the index of the first occurrence of anElement within the
	receiver. If the receiver does not contain anElement, answer 0."
	"(#(a b c d e) indexOf: #c) >>> 3"
	"(#(a b c d e) indexOf: #x) >>> 0"

	^ self indexOf: anElement ifAbsent: 0
]

{ #category : 'accessing' }
SequenceableCollection >> indexOf: anElement ifAbsent: exceptionBlock [
	"Answer the index of the first occurrence of anElement within the
	receiver. If the receiver does not contain anElement, answer the
	result of evaluating the argument, exceptionBlock."
	"(#(a b c d e) indexOf: #c ifAbsent: 7) >>> 3"
	"(#(a b c d e) indexOf: #x ifAbsent: 7) >>> 7"

	^ self indexOf: anElement startingAt: 1 ifAbsent: exceptionBlock
]

{ #category : 'accessing' }
SequenceableCollection >> indexOf: anElement ifAbsent: exceptionBlock using: comparisonBlock [
	"Answer the index of the first occurrence of anElement within the receiver. If the receiver does not contain anElement, answer the
	result of evaluating the argument, exceptionBlock."	
	"	(#(a b c d e) indexOf: #c ifAbsent: 7 using: [ : a : b | a = b ]) >>> 3. "
	" 	(#(a b c d e) indexOf: #x ifAbsent: 7 using: [ : a : b | a = b ]) >>> 7 "

	^ self indexOf: anElement startingAt: 1 ifAbsent: exceptionBlock using: comparisonBlock
]

{ #category : 'accessing' }
SequenceableCollection >> indexOf: anElement startingAt: start [
       "Answer the index of the first occurrence of anElement after start within the receiver. If the receiver does not contain anElement, answer 0."
	"(#(a b c d e) indexOf: #c startingAt: 2) >>> 3"
	"(#(a b c d e) indexOf: #c startingAt: 4) >>> 0"

	^self indexOf: anElement startingAt: start ifAbsent: 0
]

{ #category : 'accessing' }
SequenceableCollection >> indexOf: anElement startingAt: start ifAbsent: exceptionBlock [
	"Answer the index of the first occurrence of anElement after start
	within the receiver. If the receiver does not contain anElement,
	answer the 	result of evaluating the argument, exceptionBlock."
	"(#(a b c d e) indexOf: #c startingAt: 2 ifAbsent: 7) >>> 3"
	"(#(a b c d e) indexOf: #c startingAt: 4 ifAbsent: 7) >>> 7"

	start to: self size do:
		[:index |
		(self at: index) = anElement ifTrue: [^ index]].
	^ exceptionBlock value
]

{ #category : 'accessing' }
SequenceableCollection >> indexOf: anElement startingAt: start ifAbsent: exceptionBlock using: comparisonBlock [
	"Answer the index of the first occurrence of anElement after start
	within the receiver. If the receiver does not contain anElement,
	answer the 	result of evaluating the argument, exceptionBlock."
	"	(#(a b c d e) indexOf: #c startingAt: 2 ifAbsent: 7 using: [ : a : b | a = b ]) >>> 3. "
	" 	(#(a b c d e) indexOf: #x startingAt: 2 ifAbsent: 7 using: [ : a : b | a = b ]) >>> 7 "


	start to: self size do: [ : index | 
		(comparisonBlock value: (self at: index) value: anElement) 
			ifTrue: [ ^ index ] ].
	^ exceptionBlock value
]

{ #category : 'accessing' }
SequenceableCollection >> indexOfAnyOf: aCollection [
	"Answer the index of the first occurrence of any element included in aCollection within the receiver.
	If the receiver does not contain anElement, answer zero, which is an invalid index."
	"(#(a b c d e) indexOfAnyOf: #(x y c)) >>> 3"
	"(#(a b c d e) indexOfAnyOf: #(x y z)) >>> 0"

	^self indexOfAnyOf: aCollection startingAt: 1 ifAbsent: [0]
]

{ #category : 'accessing' }
SequenceableCollection >> indexOfAnyOf: aCollection ifAbsent: exceptionBlock [
	"Answer the index of the first occurrence of any element included in aCollection within the receiver.
	If the receiver does not contain anElement, answer the result of evaluating the argument, exceptionBlock."
	"(#(a b c d e) indexOfAnyOf: #(x y c) ifAbsent: 7) >>> 3"
	"(#(a b c d e) indexOfAnyOf: #(x y z) ifAbsent: 7) >>> 7"

	^self indexOfAnyOf: aCollection startingAt: 1 ifAbsent: exceptionBlock
]

{ #category : 'accessing' }
SequenceableCollection >> indexOfAnyOf: aCollection startingAt: start [
	"Answer the index of the first occurrence of any element included in aCollection after start within the receiver.
	If the receiver does not contain anElement, answer zero, which is an invalid index."
	"(#(a b c d e) indexOfAnyOf: #(x y c) startingAt: 2) >>> 3"
	"(#(a b c d e) indexOfAnyOf: #(x y c) startingAt: 4) >>> 0"

	^self indexOfAnyOf: aCollection startingAt: start ifAbsent: [0]
]

{ #category : 'accessing' }
SequenceableCollection >> indexOfAnyOf: aCollection startingAt: start ifAbsent: exceptionBlock [
	"Answer the index of the first occurrence of any element included in aCollection after start within the receiver.
	If the receiver does not contain anElement, answer the result of evaluating the argument, exceptionBlock.
	Note: it is user responsibility to provide aCollection that behaves relatevily fast when asked for includes: (like a Set)"
	"(#(a b c d e) indexOfAnyOf: #(x y c) startingAt: 2 ifAbsent: 7) >>> 3"
	"(#(a b c d e) indexOfAnyOf: #(x y c) startingAt: 4 ifAbsent: 7) >>> 7"

	start to: self size do:
		[:index |
		(aCollection includes: (self at: index)) ifTrue: [^ index]].
	^ exceptionBlock value
]

{ #category : 'accessing' }
SequenceableCollection >> indexOfSubCollection: aSubCollection startingAt: anIndex [
	"Answer the index of the receiver's first element, such that that element
	equals the first element of aSubCollection, and the next elements equal
	the rest of the elements of aSubCollection. Begin the search at element
	anIndex of the receiver. If no such match is found, answer 0."
	"(#(a b c d e) indexOfSubCollection: #(c d) startingAt: 2) >>> 3"
	"(#(a b c d e) indexOfSubCollection: #(c d) startingAt: 4) >>> 0"

	^self
		indexOfSubCollection: aSubCollection
		startingAt: anIndex
		ifAbsent: [0]
]

{ #category : 'accessing' }
SequenceableCollection >> indexOfSubCollection: sub startingAt: start ifAbsent: exceptionBlock [
	"Answer the index of the receiver's first element, such that that element
	equals the first element of sub, and the next elements equal
	the rest of the elements of sub. Begin the search at element
	start of the receiver. If no such match is found, answer the result of
	evaluating argument, exceptionBlock."
	"(#(a b c d e) indexOfSubCollection: #(c d) startingAt: 2 ifAbsent: 7) >>> 3"
	"(#(a b c d e) indexOfSubCollection: #(c d) startingAt: 4 ifAbsent: 7) >>> 7"

	| first index |
	sub isEmpty ifTrue: [^ exceptionBlock value].
	first := sub first.
	start to: self size - sub size + 1 do:
		[:startIndex |
		(self at: startIndex) = first ifTrue:
			[index := 1.
			[(self at: startIndex+index-1) = (sub at: index)]
				whileTrue:
				[index = sub size ifTrue: [^startIndex].
				index := index+1]]].
	^ exceptionBlock value
]

{ #category : 'converting' }
SequenceableCollection >> isSequenceable [
	^ true
]

{ #category : 'sorting' }
SequenceableCollection >> isSorted [
	"Return true if the receiver is sorted by the given criterion.
	Optimization for isSortedBy: [:a :b | a <= b]."
	"#(1 2 3) isSorted >>> true"
	"#(1 2 3 0) isSorted >>> false"

	| lastElm elm |
	self isEmpty ifTrue: [^ true].
	lastElm := self first.
	2 to: self size do:
		[:index |
		elm := self at: index.
		lastElm <= elm ifFalse: [^ false].
		lastElm := elm].
	^ true
]

{ #category : 'sorting' }
SequenceableCollection >> isSortedBy: aBlock [
	"Return true if the receiver is sorted by the given criterion."
	"(#(1 2 3) isSortedBy: [:a :b | a <= b ]) >>> true"
	"(#(1 2 3) isSortedBy: [:a :b | a >= b ]) >>> false"
	"(#(xa xc xz xb xy) isSortedBy: #last ascending) >>> false"
	"(#(xa xb xc xy xz) isSortedBy: #last ascending) >>> true"

	| lastElm elm |
	self isEmpty ifTrue: [^ true].
	lastElm := self first.
	2 to: self size do:
		[:index |
		elm := self at: index.
		(aBlock value: lastElm value: elm) ifFalse: [^ false].
		lastElm := elm].
	^ true
]

{ #category : 'splitjoin' }
SequenceableCollection >> join: aCollection [
	"Append the elements of the argument, aSequenceableCollection, separating them by the receiver."

	"({Character space. Character space} join: #('Pharo' 'is' 'cool')) >>>  {$P. $h. $a. $r. $o. Character space. Character space. $i. $s. Character space. Character space. $c. $o. $o. $l}"

	"NB: this implementation only works for Array, since WriteStreams only work for Arrays and Strings. (!) Overridden in OrderedCollection and SortedCollection."

	^ self class
		streamContents: [:stream | aCollection
				do: [:each | each joinTo: stream]
				separatedBy: [stream nextPutAll: self]]
]

{ #category : 'splitjoin' }
SequenceableCollection >> joinTo: stream [
	"double dispatch for join:"
	^ stream nextPutAll: self
]

{ #category : 'splitjoin' }
SequenceableCollection >> joinUsing: joiner [
	"Append the elements of the receiver separating them with the joiner - character, string or sequenceable collection. Return collection of the same collection class as 'joiner', or a String"
	^ joiner join: self
]

{ #category : 'splitjoin' }
SequenceableCollection >> joinUsing: joiner last: last [
	"Append the elements of the receiver separating them with the joiner argument and handle the last elment separation using the last argument."

	"(#(1 2 3 4) joinUsing: ', ' last: ' and ') >>> '1, 2, 3 and 4'"

	(self size = 0) ifTrue: [  ^ '' ].
	(self size = 1) ifTrue: [  ^ self first asString ].
	^ last join: (Array
				with: (joiner join: self allButLast)
				with: self last)
]

{ #category : 'enumerating' }
SequenceableCollection >> keysAndValuesDo: aBlock [
	"Enumerate the receiver with all the keys (aka indices) and values."

	"(Array streamContents: [:stream | #(10 20 30) keysAndValuesDo:  [:key :value | stream nextPut: (key * 2 + value)]]) >>> #(12 24 36)"

	1 to: self size do: [:index | aBlock value: index value: (self at: index)]
]

{ #category : 'accessing' }
SequenceableCollection >> last [
	"Answer the last element of the receiver"
	"#(a b c d e) last >>> #e"

	^ self at: self size
]

{ #category : 'accessing' }
SequenceableCollection >> last: n [
	"Answer the last n elements of the receiver.
	Raise an error if there are not enough elements."
	"(#(a b c d e) last: 2) >>> #(d e)"
	"(#(a b c d e) last: 3) >>> #(c d e)"

	| size |
	size := self size.
	^ self copyFrom: size - n + 1 to: size
]

{ #category : 'accessing' }
SequenceableCollection >> lastIndexOf: anElement [
	"Answer the index of the last occurrence of anElement within the
	receiver. If the receiver does not contain anElement, answer 0."
	"(#(a b a d a) lastIndexOf: #a) >>> 5"
	"(#(a b a d e) lastIndexOf: #a) >>> 3"

	^ self lastIndexOf: anElement startingAt: self size ifAbsent: [0]
]

{ #category : 'accessing' }
SequenceableCollection >> lastIndexOf: anElement ifAbsent: exceptionBlock [
	"Answer the index of the last occurrence of anElement within the
	receiver. If the receiver does not contain anElement, answer the
	result of evaluating the argument, exceptionBlock."
	"(#(a b a d a) lastIndexOf: #a ifAbsent: 7) >>> 5"
	"(#(a b a d e) lastIndexOf: #c ifAbsent: 7) >>> 7"

	^self lastIndexOf: anElement startingAt: self size ifAbsent: exceptionBlock
]

{ #category : 'accessing' }
SequenceableCollection >> lastIndexOf: anElement startingAt: lastIndex ifAbsent: exceptionBlock [
	"Answer the index of the last occurrence of anElement within the
	receiver. If the receiver does not contain anElement, answer the
	result of evaluating the argument, exceptionBlock."
	"(#(a b a d a) lastIndexOf: #a startingAt: 1 ifAbsent: 7) >>> 1"
	"(#(e b a d e) lastIndexOf: #a startingAt: 4 ifAbsent: 7) >>> 3"

	lastIndex to: 1 by: -1 do:
		[:index |
		(self at: index) = anElement ifTrue: [^ index]].
	^ exceptionBlock value
]

{ #category : 'accessing' }
SequenceableCollection >> lastIndexOfAnyOf: aCollection startingAt: lastIndex ifAbsent: exceptionBlock [
	"Answer the index of the last occurrence of anElement within the
	receiver. If the receiver does not contain anElement, answer the
	result of evaluating the argument, exceptionBlock."
	"(#(a b a d a) lastIndexOfAnyOf: #(a b) startingAt: 1 ifAbsent: 7) >>> 1"

	lastIndex to: 1 by: -1 do:
		[:index |
		(aCollection includes: (self at: index)) ifTrue: [^ index]].
	^ exceptionBlock value
]

{ #category : 'sorting' }
SequenceableCollection >> mergeFirst: first middle: middle last: last into: dst by: aBlock [
	"Private. Merge the sorted ranges [first..middle] and [middle+1..last]
	of the receiver into the range [first..last] of dst."

	| i1 i2 val1 val2 out |
	i1 := first.
	i2 := middle + 1.
	val1 := self at: i1.
	val2 := self at: i2.
	out := first - 1.  "will be pre-incremented"

	"select 'lower' half of the elements based on comparator"
	[(i1 <= middle) and: [i2 <= last]] whileTrue:
		[(aBlock value: val1 value: val2)
			ifTrue: [dst at: (out := out + 1) put: val1.
					val1 := self at: (i1 := i1 + 1)]
			ifFalse: [dst at: (out := out + 1) put: val2.
					i2 := i2 + 1.
					i2 <= last ifTrue: [val2 := self at: i2]]].

	"copy the remaining elements"
	i1 <= middle
		ifTrue: [dst replaceFrom: out + 1 to: last with: self startingAt: i1]
		ifFalse: [dst replaceFrom: out + 1 to: last with: self startingAt: i2]
]

{ #category : 'sorting' }
SequenceableCollection >> mergeSortFrom: startIndex to: stopIndex by: aBlock [
	"Sort the given range of indices using the mergesort algorithm.
	Mergesort is a worst-case O(N log N) sorting algorithm that usually
	does only half as many comparisons as heapsort or quicksort."

	"Details: recursively split the range to be sorted into two halves,
	mergesort each half, then merge the two halves together. An extra
	copy of the data is used as temporary storage and successive merge
	phases copy data back and forth between the receiver and this copy.
	The recursion is set up so that the final merge is performed into the
	receiver, resulting in the receiver being completely sorted."
	"({#a. #b. #z. #d. #i. #l} mergeSortFrom: 3 to: 5 by: [ :a :b | a<=b ]) >>> #(a b d i z l)"

	self size <= 1 ifTrue: [^ self].  "nothing to do"
	startIndex = stopIndex ifTrue: [^ self].
	[startIndex >= 1 and: [startIndex < stopIndex]] assert. "bad start index"
	[stopIndex <= self size] assert. "bad stop index"
	self
		mergeSortFrom: startIndex
		to: stopIndex
		src: self copy
		dst: self
		by: aBlock
]

{ #category : 'sorting' }
SequenceableCollection >> mergeSortFrom: first to: last src: src dst: dst by: aBlock [
	"Private. Split the range to be sorted in half, sort each half, and
	merge the two half-ranges into dst."

	| middle |
	first = last ifTrue: [^ self].
	middle := (first + last) // 2.
	self mergeSortFrom: first to: middle src: dst dst: src by: aBlock.
	self mergeSortFrom: middle + 1 to: last src: dst dst: src by: aBlock.
	src mergeFirst: first middle: middle last: last into: dst by: aBlock
]

{ #category : 'accessing' }
SequenceableCollection >> middle [
	"Answer the middle element of the receiver."
	"#(a b c d e) middle >>> #c"
	"#(a b c d) middle >>> #c"

	^ self at: self size // 2 + 1
]

{ #category : 'accessing' }
SequenceableCollection >> nextToLast [
	"(#(1 2 3 4) nextToLast) >>> 3"
	^self at: self size - 1
]

{ #category : 'accessing' }
SequenceableCollection >> ninth [
	"Answer the ninth element of the receiver.
	Raise an error if there are not enough elements."
	"#(a b c d e f g h i) ninth >>> #i"

	^ self at: 9
]

{ #category : 'enumerating' }
SequenceableCollection >> overlappingPairsCollect: aBlock [

	"Answer the result of evaluating aBlock with all of the overlapping pairs of my elements."

	"(#(1 2 3 4) overlappingPairsCollect: [:first :second| first + second]) >>> #(3 5 7)"

	| retval |
	retval := self species ofSize: self size - 1.
	1 to: self size - 1
		do: [:i | retval at: i put: (aBlock value: (self at: i) value: (self at: i + 1)) ].
	^retval
]

{ #category : 'enumerating' }
SequenceableCollection >> overlappingPairsDo: aBlock [

	"Emit overlapping pairs of my elements into aBlock"

	"(Array streamContents: [:stream | #(1 2 3 4) overlappingPairsDo: [:first :second| stream nextPut: (first + second)]]) >>> #(3 5 7)"

	1 to: self size - 1
		do: [:i | aBlock value: (self at: i) value: (self at: i + 1)]
]

{ #category : 'enumerating' }
SequenceableCollection >> overlappingPairsWithIndexDo: aBlock [
	"Emit overlapping pairs of my elements into aBlock, along with an index."

	"(Array streamContents: [:stream | #(10 20 30 40) overlappingPairsWithIndexDo: [:first :second :index| stream nextPut: (first + second + index)]]) >>> #(31 52 73)"

	1 to: self size - 1
		do: [:i | aBlock value: (self at: i) value: (self at: i + 1) value: i ]
]

{ #category : 'enumerating' }
SequenceableCollection >> paddedWith: otherCollection do: twoArgBlock [
	"Evaluate twoArgBlock with corresponding elements from this collection and otherCollection.
	Missing elements from either will be passed as nil."

	"(Array streamContents: [:stream | #(10 20 30) paddedWith: #(40 50 60) do: [:first :second | stream nextPut: (first + second)]]) >>> #(50 70 90) "

	"(Array streamContents: [:stream | #(10 20 30) paddedWith: #(40 50) do: [:a :b | stream nextPut: {a. b} ]]) >>> #(#(10 40) #(20 50) #(30 nil)) "

	"(Array streamContents: [:stream | #(10 20) paddedWith: #(40 50 60) do: [:a :b | stream nextPut: {a. b} ]]) >>> #(#(10 40) #(20 50) #(nil 60))"

	1 to: (self size max: otherCollection size) do:
		[:index | twoArgBlock value: (self at: index ifAbsent: [])
				value: (otherCollection at: index ifAbsent: [])]
]

{ #category : 'enumerating' }
SequenceableCollection >> pairsCollect: aBlock [
	"Evaluate aBlock with my elements taken two at a time, and return an Array with the results"

	"(#(1 'fred' 2 'charlie' 3 'elmer') pairsCollect: [:a :b | b, ' is number ', a printString]) >>> #('fred is number 1' 'charlie is number 2' 'elmer is number 3')"

	^ (1 to: self size // 2)
		collect: [ :index | aBlock value: (self at: 2 * index - 1) value: (self at: 2 * index) ]
]

{ #category : 'enumerating' }
SequenceableCollection >> pairsDo: aBlock [
	"Evaluate aBlock with my elements taken two at a time.  If there's an odd number of items, ignore the last one.  Allows use of a flattened array for things that naturally group into pairs.  See also pairsCollect:"

	"(Array streamContents: [:s | #(1 'fred' 2 'charlie' 3 'elmer') pairsDo: [:a :b | s nextPut: b; nextPut: a]]) >>> #('fred' 1 'charlie' 2 'elmer' 3)"

	1 to: self size // 2 do: [ :index | aBlock value: (self at: 2 * index - 1) value: (self at: 2 * index) ]
]

{ #category : 'enumerating' }
SequenceableCollection >> permutationsDo: aBlock [
	"Repeatly value aBlock with a single copy of the receiver. Reorder the copy
	so that aBlock is presented all (self size factorial) possible permutations."

	"(Array streamContents: [:stream | (1 to: 3) permutationsDo: [:each | stream nextPut: each copy]]) >>> #(#(1 2 3) #(1 3 2) #(2 1 3) #(2 3 1) #(3 2 1) #(3 1 2))"

	self shallowCopy permutationsStartingAt: 1 do: aBlock
]

{ #category : 'private' }
SequenceableCollection >> permutationsStartingAt: anInteger do: aBlock [
	anInteger > self size
		ifTrue: [ ^ self ].
	anInteger = self size
		ifTrue: [ ^ aBlock value: self ].
	anInteger to: self size do: [ :i |
		self swap: anInteger with: i.
		self permutationsStartingAt: anInteger + 1 do: aBlock.
		self swap: anInteger with: i ]
]

{ #category : 'enumerating' }
SequenceableCollection >> permutationsWithRepetitionOfSize: aSize subSize: sSize in: anArray do: aBlock [

 	aSize < sSize ifTrue: [ ^ self error: 'sSize cannot exceed array size' ].
 	sSize < 1 ifTrue: [ ^ self error: 'sizes must be positive' ].
 	sSize = 1
 		ifTrue: [
 			self do: [ :each |
 				aBlock value: (anArray
 						 at: aSize - sSize + 1 put: each;
 						 yourself) ] ]
 		ifFalse: [
 			self do: [ :each |
 				self
 					permutationsWithRepetitionOfSize: aSize
 					subSize: sSize - 1
 					in: anArray
 					do: [ :eachArray |
 						aBlock value: (eachArray
 								 at: aSize - sSize + 1 put: each;
 								 yourself) ] ] ]
]

{ #category : 'enumerating' }
SequenceableCollection >> permutationsWithRepetitionsDo: aBlock [
	" Iterate over all receiver's possible combinations of its size, including repetitions."

	self permutationsWithRepetitionsOfSize: self size do: aBlock
]

{ #category : 'enumerating' }
SequenceableCollection >> permutationsWithRepetitionsOfSize: aSize [
	"Answer a <Collection> with all the possible combinations of aSize numbers including repetitions."
   | result |

   result := OrderedCollection new: aSize.
   self 
		permutationsWithRepetitionsOfSize: aSize
		do: [ : each | result add: (each joinUsing: self class empty) ].
	^ result
]

{ #category : 'enumerating' }
SequenceableCollection >> permutationsWithRepetitionsOfSize: aSize do: aBlock [
 	"See comment in #enumerationsOfSize: "

   | anArray |

   anArray := Array new: aSize.
   self permutationsWithRepetitionOfSize: aSize subSize: aSize in: anArray do: [ : each | aBlock value: each ]
]

{ #category : 'enumerating' }
SequenceableCollection >> piecesCutWhere: binaryBlock [
	"Answer substrings of the receiver derived from cutting the receiver at points where binaryBlock answers true for adjacent elements."
	"( #(1 2 3 1 6 1 2 3 4) piecesCutWhere: [ :a :b | a = 1 and: [ b = 2]]) asArray >>> #(#(1) #(2 3 1 6 1) #(2 3 4))"
	| pieces |
	pieces := OrderedCollection new.
	self
		piecesCutWhere: binaryBlock
		do: [ :piece | pieces add: piece ].
	^pieces

   "'Now is the time for all good people to come to the aid of the cause of world peace.  It is just fine, even desirable, to love your country, if that means wanting it to play a beneficial role in the course of world events and be the best possible example of a good society.  But if it means wanting dominion over the rest of the world, it is not love but defensiveness or self-glorification, and will lead only to oblivion.'  piecesCutWhere: [:a :b| a = $. and: [b isSeparator]]"
]

{ #category : 'enumerating' }
SequenceableCollection >> piecesCutWhere: binaryBlock do: pieceBlock [
	"Evaluate pieceBlock with substrings of the receiver derived from cutting the receiver at points where binaryBlock answers true for adjacent elements."

	"(Array streamContents: [:stream |  #(1 2 3 1 6 1 2 3 4) piecesCutWhere: [ :a :b | a = 1 and: [ b = 2]] do: [:each | stream nextPut: each size]]) >>> #(1 5 3)"

	| size lastCut this next |
	(size := self size) <= 1 ifTrue:
		 [size = 1 ifTrue: [pieceBlock value: self].
		^self].
	lastCut := 1.
	this := self at: 1.
	2 to: size do:
		[:i|
		next := self at: i.
		(binaryBlock value: this value: next) ifTrue:
			[pieceBlock value: (self copyFrom: lastCut to: i - 1).
			lastCut := i].
		this := next].
	pieceBlock value: (self copyFrom: lastCut to: size)
]

{ #category : 'enumerating' }
SequenceableCollection >> polynomialEval: thisX [
	"Treat myself as the coeficients of a polynomial in X.  Evaluate it with thisX.  First element is the constant and last is the coeficient for the highest power.
	#(1 2 3) polynomialEval: 2  is 3*X^2 + 2*X + 1 with X = 2"

	"(#(1 2 3) polynomialEval: 2) >>> 17"

	| sum valToPower |
	sum := self first.
	valToPower := thisX.
	2 to: self size do: [ :ind |
		sum := sum + ((self at: ind) * valToPower).
		valToPower := valToPower * thisX ].
	^ sum
]

{ #category : 'streaming' }
SequenceableCollection >> putOn: aStream [
	"Write the receiver onto aStream by iterating over its elements.
	In general we assume aStream accepts the receiver's elements as element type.
	Return self."

	self do: [ :each | each putOn: aStream ]
]

{ #category : 'streaming' }
SequenceableCollection >> readStreamDo: aBlock [
	"Evaluates the argument with the read stream of the collection. Answers the result."

	"(#(3 4 5) readStreamDo: [ :stream | stream contents ]) >>> #(3 4 5)"

	^ aBlock value: self readStream
]

{ #category : 'enumerating' }
SequenceableCollection >> reduce: aBlock [
	"Fold the result of the receiver into aBlock. The argument aBlock must take two or more arguments. It applies the argument, binaryBlock cumulatively to the elements of the receiver. For sequenceable collections the elements will be used in order, for unordered collections the order is unspecified."

	"(#(1 2 3) reduce: [ :a :b | a + b ]) >>> 6" "1 + 2 + 3"
	"(#(1 2 3) reduce: [ :a :b | a + b ]) >>> (1 + 2 + 3)"

	"(#(1 2 3 4 5) reduce: [ :a :b :c | a + b + c ]) >>> 15"
	"(#(1 2 3 4 5) reduce: [ :a :b :c | a + b + c ]) >>> (1 + 2 + 3 + 4 + 5)"

	^ self reduceLeft: aBlock
]

{ #category : 'enumerating' }
SequenceableCollection >> reduceLeft: aBlock [
	"Fold the result of the receiver from left to right into aBlock. The argument aBlock must take two or more arguments."

	"(#(1 2 3) reduceLeft: [ :a :b | a - b ])>>> ((1 - 2) - 3)"
	"(#(1 2 3) reduceLeft: [ :a :b | a - b ]) >>> -4"
	"(#(1 + 3 - 5) reduceLeft: [ :a :op :b | a perform: op with: b ]) >>> ((1 + 3) - 5)"
	"(#(1 + 3 - 5) reduceLeft: [ :a :op :b | a perform: op with: b ]) >>> -1"

	| arguments |
	self emptyCheck.
	arguments := Array new: aBlock argumentCount.
	(arguments size = 0 or: [ (self size + 1) \\ (arguments size - 1) > 0 ])
		ifTrue: [ self error: 'Collection size and block argument count do not match.' ].
	arguments at: 1 put: self first.
	2 to: self size by: arguments size - 1 do: [ :index |
		arguments
			replaceFrom: 2 to: arguments size with: self startingAt: index;
			at: 1 put: (aBlock valueWithArguments: arguments) ].
	^ arguments first
]

{ #category : 'enumerating' }
SequenceableCollection >> reduceRight: aBlock [
	"Fold the result of the receiver from right to left into aBlock. The argument aBlock must take two or more arguments."

	"(#(1 2 3) reduceRight: [ :a :b | a - b ]) >>> (1 - (2 - 3))"
	"(#(1 2 3) reduceRight: [ :a :b | a - b ]) >>> 2"
	"(#(1 + 3 - 5) reduceRight: [ :a :op :b | a perform: op with: b ]) >>> (1 + (3 - 5))"
	"(#(1 + 3 - 5) reduceRight: [ :a :op :b | a perform: op with: b ]) >>> -1"

	| arguments |
	self emptyCheck.
	arguments := Array new: aBlock argumentCount.
	(arguments size = 0 or: [ (self size + 1) \\ (arguments size - 1) > 0 ])
		ifTrue: [ self error: 'Collection size and block argument count do not match.' ].
	arguments at: arguments size put: self last.
	self size - arguments size + 1 to: 1 by: 1 - arguments size do: [ :index |
		arguments
			replaceFrom: 1 to: aBlock argumentCount - 1 with: self startingAt: index;
			at: arguments size put: (aBlock valueWithArguments: arguments) ].
	^ arguments last
]

{ #category : 'enumerating' }
SequenceableCollection >> reject: rejectBlock [
	"Optimized version of Collection>>#reject:"

	"(#(1 2 3 4) reject: [:each | each = 3 ]) >>> #(1 2 4)"

	| each |

	^ self class new: self size streamContents: [ :stream|
		1 to: self size do: [ :index |
			(rejectBlock value: (each := self at: index))
				ifFalse: [ stream nextPut: each ]]]
]

{ #category : 'enumerating' }
SequenceableCollection >> reject: rejectBlock thenCollect: collectBlock [
	"Optimized version of Collection>>#reject:thenCollect:"

	"(#(1 2 3 4) reject: [:each | each = 3 ] thenCollect: [:each | each + 10 ]) >>> #(11 12 14)"

	| each |

	^ self class new: self size streamContents: [ :stream|
		1 to: self size do: [ :index |
			(rejectBlock value: (each := self at: index))
				ifFalse: [ stream nextPut: (collectBlock value: each) ]]]
]

{ #category : 'enumerating' }
SequenceableCollection >> reject: rejectBlock thenDo: aBlock [
	"Refer to the comment in Collection>>#reject:thenDo:"

	"(Array streamContents: [:stream | #(1 2 3 4) reject: [:each | each = 3 ] thenDo: [:each | stream nextPut: each + 10]]) >>> #(11 12 14)"

	|  each |

	1 to: self size do: [ :index |
		(rejectBlock value: (each := self at: index))
			ifFalse: [ aBlock value: each ]]
]

{ #category : 'removing' }
SequenceableCollection >> remove: oldObject ifAbsent: anExceptionBlock [
	"SequencableCollections cannot implement removing."

	self shouldNotImplement
]

{ #category : 'enumerating' }
SequenceableCollection >> replace: aBlock [
	"Evaluate aBlock with each of the receiver's elements as the argument.
	Collect the resulting values into self."

	"({1. 2. 3. 4} replace: [:each | each + 1 ]) >>> #(2 3 4 5)"

	1 to: self size do: [ :index |
		self at: index put: (aBlock value: (self at: index)) ]
]

{ #category : 'accessing' }
SequenceableCollection >> replaceAll: oldObject with: newObject [
	"Replace all occurrences of oldObject with newObject"
	"({#a. #b. #a. #d. #a} replaceAll: #a with: #e) >>> #(e b e d e)"

	| index |
	index := self
				indexOf: oldObject
				startingAt: 1
				ifAbsent: [0].
	[index = 0]
		whileFalse:
			[self at: index put: newObject.
			index := self
						indexOf: oldObject
						startingAt: index + 1
						ifAbsent: [0]]
]

{ #category : 'accessing' }
SequenceableCollection >> replaceFrom: start to: stop with: replacement [
	"This destructively replaces elements from start to stop in the receiver.
	Answer the receiver itself. Use copyReplaceFrom:to:with: for
	insertion/deletion which may alter the size of the result."
	"({#a. #b. #c. #d. #e} replaceFrom: 3 to: 4 with: #(x y)) >>> #(a b x y e)"

	replacement size = (stop - start + 1)
		ifFalse: [self error: 'Size of replacement doesnt match'].
	^self replaceFrom: start to: stop with: replacement startingAt: 1
]

{ #category : 'accessing' }
SequenceableCollection >> replaceFrom: start to: stop with: replacement startingAt: repStart [
	"This destructively replaces elements from start to stop in the receiver
	starting at index, repStart, in the sequenceable collection,
	replacementCollection. Answer the receiver. No range checks are
	performed."

	| index repOff |
	repOff := repStart - start.
	index := start - 1.
	[(index := index + 1) <= stop]
		whileTrue: [self at: index put: (replacement at: repOff + index)]
]

{ #category : 'converting' }
SequenceableCollection >> reverse [
	"Answer a copy of the receiver with element order reversed, as expected by ANSI."

	^ self reversed
]

{ #category : 'enumerating' }
SequenceableCollection >> reverseDo: aBlock [
	"Evaluate aBlock with each of the receiver's elements as the argument,
	starting with the last element and taking each in sequence up to the
	first. For SequenceableCollections, this is the reverse of the enumeration
	for do:."

	"(Array streamContents: [:stream | #(1 2 3) reverseDo: [:each | stream nextPut: each + 10]]) >>> #(13 12 11)"

	self size to: 1 by: -1 do: [:index | aBlock value: (self at: index)]
]

{ #category : 'enumerating' }
SequenceableCollection >> reverseWith: aSequenceableCollection do: aBlock [
	"Evaluate aBlock with each of the receiver's elements, in reverse order,
	along with the  corresponding element, also in reverse order, from
	aSequencableCollection. "

	"(Array streamContents: [:stream | #(1 2 3) reverseWith: #(4 5 6) do: [:a :b | stream nextPut: (a + b)]]) >>> #(9 7 5)"

	self size ~= aSequenceableCollection size ifTrue: [^ self errorSizeMismatch].
	self size to: 1 by: -1 do: [:index |
			aBlock
				value: (self at: index)
				value: (aSequenceableCollection at: index)]
]

{ #category : 'enumerating' }
SequenceableCollection >> reverseWithIndexDo: elementAndIndexBlock [
	"Just like reverseWith:do: except that the iteration index supplies the second argument to the block."

	"(Array streamContents: [:stream | #(10 20 30) reverseWithIndexDo: [:each :index | stream nextPut: each + index]]) >>> #(33 22 11)"

	self size to: 1 by: -1 do: [:index |
		elementAndIndexBlock
			value: (self at: index)
			value: index]
]

{ #category : 'converting' }
SequenceableCollection >> reversed [
	"Answer a copy of the receiver with element order reversed."
	"Example: 'frog' reversed"

	| n result src |
	n := self size.
	result := self species new: n.
	src := n + 1.
	1 to: n do: [:i | result at: i put: (self at: (src := src - 1))].
	^ result
]

{ #category : 'accessing' }
SequenceableCollection >> second [
	"Answer the second element of the receiver.
	Raise an error if there are not enough elements."
	"#(a b c d e f g h i) second >>> #b"

	^ self at: 2
]

{ #category : 'enumerating' }
SequenceableCollection >> select: aBlock [
	"Optimized version of Collection>>#select:"

	"(#(1 2 3 4) select: [:each | each > 2 ]) >>> #(3 4)"

	| each |
	^ self species new: self size streamContents: [ :stream|
		1 to: self size do: [ :index |
			(aBlock value: (each := self at: index))
				ifTrue: [ stream nextPut: each ]]]
]

{ #category : 'enumerating' }
SequenceableCollection >> select: selectBlock thenCollect: collectBlock [
	"Optimized version of Collection>>#select:thenCollect:"

	"(#(1 2 3 4)  select: [:each | each > 2 ] thenCollect: [:each | each + 10 ]) >>> #(13 14)"

	| each |
	^ self class new: self size streamContents: [ :stream|
		1 to: self size do: [ :index |
			(selectBlock value: (each := self at: index))
				ifTrue: [ stream nextPut: (collectBlock value: each) ]]]
]

{ #category : 'enumerating' }
SequenceableCollection >> select: selectBock thenDo: aBlock [
	"Refer to the comment in Collection>>#select:thenDo:"

	"(Array streamContents: [:stream | #(1 2 3 4) select: [:each | each > 2 ] thenDo: [:each | stream nextPut: each + 10]]) >>> #(13 14)"

	| each |
	1 to: self size do: [ :index |
		(selectBock value: (each := self at: index))
			ifTrue: [ aBlock value: each ]]
]

{ #category : 'accessing' }
SequenceableCollection >> seventh [
	"Answer the seventh element of the receiver.
	Raise an error if there are not enough elements."
	"#(a b c d e f g h i) seventh >>> #g"

	^ self at: 7
]

{ #category : 'accessing' }
SequenceableCollection >> sixth [
	"Answer the sixth element of the receiver.
	Raise an error if there are not enough elements."
	"#(a b c d e f g h i) sixth >>> #f"

	^ self at: 6
]

{ #category : 'accessing' }
SequenceableCollection >> size [
	"#(a b c d e) size >>> 5"
	"#(x y z) size >>> 3"

	^ self subclassResponsibility
]

{ #category : 'sorting' }
SequenceableCollection >> sort [
	"Sort this collection into ascending order using the '<=' operator."
	"{8. 5. 3. 9} sort >>> #(3 5 8 9)"
	"{#a. #b. #z. #d} sort >>> #(a b d z)"

	self sort: [:a :b | a <= b]
]

{ #category : 'sorting' }
SequenceableCollection >> sort: aSortBlock [
	"Sort this array using aSortBlock. The block should take two arguments
	and return true if the first element should preceed the second one."
	"({3. 9. 1} sort: [:a :b | a <= b ]) >>> #(1 3 9)"
	"({3. 9. 1} sort: [:a :b | a >= b ]) >>> #(9 3 1)"
	"({#xa. #xc. #xz. #xb. #xy} sort: #last ascending) >>> #(xa xb xc xy xz)"

	self
		mergeSortFrom: 1
		to: self size
		by: aSortBlock
]

{ #category : 'sorting' }
SequenceableCollection >> sorted [
	"Return a new sequenceable collection which contains the same elements as self but its
elements are sorted in ascending order using the #'<=' operator."
	"#(8 5 3 9) sorted >>> #(3 5 8 9)"
	"#(a b z d) sorted >>> #(a b d z)"

	^self sorted: [ :a :b| a <= b ]
]

{ #category : 'sorting' }
SequenceableCollection >> sorted: aSortBlockOrNil [
	"Return a new sequenceable collection which contains the same elements as self but its
elements are sorted by aSortBlockOrNil. The block should take two arguments and return true if
the first element should preceed the second one. If aSortBlock is nil then <= is used for
comparison."
	"(#(3 9 1) sorted: [:a :b | a <= b ]) >>> #(1 3 9)"
	"(#(3 9 1) sorted: [:a :b | a >= b ]) >>> #(9 3 1)"
	"(#(xa xc xz xb xy) sorted: #last ascending) >>> #(xa xb xc xy xz)"

	^self copy sort: aSortBlockOrNil
]

{ #category : 'splitjoin' }
SequenceableCollection >> split: aSequenceableCollection indicesDo: aBlock [
	"Perform an action specified as aBlock (with a start and end argument) to each of the indices of aSequenceableCollection that have been identified by taking the receiver as a splitter."
	"('||' split: 'foo||bar||2')>>>#('foo' 'bar' '2') asOrderedCollection"
	"(String streamContents: [:s | '||' split: 'foo||bar||2' indicesDo: [ :start :end | s << 's:' << start asString << ' ' << 'e:' << end asString << ' ' ]]) >>> 's:1 e:3 s:6 e:8 s:11 e:11 '"

	| position oldPosition |
	position := 1.
	oldPosition := position.
	position := aSequenceableCollection indexOfSubCollection: self startingAt: position.

	[ position > 0 ] whileTrue: [
		aBlock value: oldPosition value: position - 1.
		position := position + self size.
		oldPosition := position.
		position := aSequenceableCollection indexOfSubCollection: self startingAt: position.
	].

	aBlock value: oldPosition value: aSequenceableCollection size
]

{ #category : 'splitjoin' }
SequenceableCollection >> splitOn: splitter [
	"Split a collection of objects based on a splitter. splitter - can be a subsequence, a Block or a Regex (String receiver only). Any other object used as a splitter is treated as an Array containing that object."

	"(#(1 2 3 3 4 1 2 3 5 4 6) splitOn: 4) >>> #(#(1 2 3 3) #(1 2 3 5) #(6))asOrderedCollection"
	"(#(1 2 3 3 4 1 2 3 3 5 6) splitOn: #(3 3)) >>> #(#(1 2) #(4 1 2) #(5 6)) asOrderedCollection"
	"(#(2 2 3 3 4 1 2 3 3 5) splitOn: [:each | each > 3])>>> #(#(2 2 3 3) #(1 2 3 3) #()) asOrderedCollection"

	^ splitter split: self
]

{ #category : 'splitjoin' }
SequenceableCollection >> splitOn: splitter do: aBlock [
	"Perform an action specified as aBlock to each of the elements of the receiver that have been split using the splitter argument. splitter - can be a subsequence, a Block or a Regex (String receiver only). Any other object used as a splitter is treated as an Array containing that object."
	"(String streamContents: [:s | 'Pharo is cool' splitOn: Character space do: [:each | s << each ]])>>>'Pharoiscool'"

	^ splitter split: self do: aBlock
]

{ #category : 'splitjoin' }
SequenceableCollection >> splitOn: splitter indicesDo: aBlock [
	"Perform an action specified as aBlock (with a start and end argument) to each of the indices of the receiver element that have been identified by splitting the receiver using the splitter argument. splitter - can be a subsequence, a Block or a Regex (String receiver only). Any other object used as a splitter is treated as an Array containing that object."
	"(String streamContents: [:s | 'Pharo is cool' splitOn: Character space indicesDo: [ :start :end | s << 's:' << start asString << ' ' << 'e:' << end asString << ' ' ]]) >>> 's:1 e:5 s:7 e:8 s:10 e:13 '"


	^ splitter split: self indicesDo: aBlock
]

{ #category : 'accessing' }
SequenceableCollection >> swap: oneIndex with: anotherIndex [
	"Move the element at oneIndex to anotherIndex, and vice-versa."
	"({#a. #b. #c. #d. #e} swap: 5 with: 1) >>> #(e b c d a)"

	| element |
	element := self at: oneIndex.
	self at: oneIndex put: (self at: anotherIndex).
	self at: anotherIndex put: element
]

{ #category : 'accessing' }
SequenceableCollection >> takeFirst: anInteger [

	^ self first: (self size min: anInteger)
]

{ #category : 'accessing' }
SequenceableCollection >> third [
	"Answer the third element of the receiver.
	Raise an error if there are not enough elements."
	"#(a b c d e f g h i) third >>> #c"

	^ self at: 3
]

{ #category : 'copying' }
SequenceableCollection >> truncatedToSize: smallSize [
	"return myself or a copy shortened to smallSize."

	^ self size <= smallSize
		ifTrue: [ self ]
		ifFalse: [ self copyFrom: 1 to: smallSize ]
]

{ #category : 'enumerating' }
SequenceableCollection >> with: otherCollection collect: twoArgBlock [
	"Collect and return the result of evaluating twoArgBlock with corresponding elements from this collection and otherCollection."

	"(#(1 2 3) with: #(4 5 6) collect: [:a :b | a + b]) >>> #(5 7 9)"

	| result |
	otherCollection size = self size ifFalse: [self errorSizeMismatch].
	result := self species new: self size.
	1 to: self size do:
		[:index | result at: index put:
		(twoArgBlock
			value: (self at: index)
			value: (otherCollection at: index))].
	^ result
]

{ #category : 'enumerating' }
SequenceableCollection >> with: otherCollection do: twoArgBlock [
	"Evaluate twoArgBlock with corresponding elements from this collection and otherCollection."

	"(Array streamContents: [:stream | #(1 2 3) with: #(4 5 6) do: [:a :b | stream nextPut: (a + b)]]) >>> #(5 7 9)"

	otherCollection size = self size ifFalse: [self errorSizeMismatch].
	1 to: self size do:
		[:index |
		twoArgBlock value: (self at: index)
				value: (otherCollection at: index)]
]

{ #category : 'enumerating' }
SequenceableCollection >> withIndexCollect: elementAndIndexBlock [
	"Just like with:collect: except that the iteration index supplies the second argument to the block."

	"(#(10 20 30) withIndexCollect: [:each :index | each + (index * 2)]) >>> #(12 24 36)"

	| result |
	result := self species new: self size.
	1 to: self size do:
		[:index | result at: index put:
		(elementAndIndexBlock
			value: (self at: index)
			value: index)].
	^ result
]

{ #category : 'enumerating' }
SequenceableCollection >> withIndexDo: elementAndIndexBlock [
	"Just like do: except that the iteration index supplies the second argument to the block"

	"(Array streamContents: [:stream| #(11 22 13) withIndexDo: [ :each :i | stream nextPut: (each * each + i)]]) >>> #(122 486 172)"

	1 to: self size do: [ :index | elementAndIndexBlock value: (self at: index) value: index ]
]

{ #category : 'enumerating' }
SequenceableCollection >> withIndexSelect: elementAndIndexBlock [
	"select elements from the receiver that the block evaluates true with the element and its index."

	"(#('We' 'love' 'pharo!') withIndexSelect: [:value :index | value size - 1 <= index]) >>> #('We')"

	^ self class
		new: self size
		streamContents: [ :stream |
			1 to: self size do: [ :index |
				| each |
				(elementAndIndexBlock value: (each := self at: index) value: index)
					ifTrue: [ stream nextPut: each ] ] ]
]
