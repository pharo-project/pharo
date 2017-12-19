"
I am an abstract superclass for collections that have a well-defined order associated with their elements. Thus each element is externally-named by integers referred to as indices.
"
Class {
	#name : #SequenceableCollection,
	#superclass : #Collection,
	#category : #'Collections-Abstract'
}

{ #category : #'stream creation' }
SequenceableCollection class >> << blockWithArg [
	^ self streamContents: blockWithArg
]

{ #category : #'stream creation' }
SequenceableCollection class >> new: newSize streamContents: blockWithArg [
	| stream |
	stream := WriteStream on: (self streamSpecies new: newSize).
	blockWithArg value: stream.
	^ stream contents
]

{ #category : #'instance creation' }
SequenceableCollection class >> ofSize: n [
	"Create a new collection of size n with nil as its elements.
	This method exists because OrderedCollection new: n creates an
	empty collection,  not one of size n."
	^ self new: n
]

{ #category : #'stream creation' }
SequenceableCollection class >> streamContents: blockWithArg [

	^ self new: 100 streamContents: blockWithArg
]

{ #category : #'stream creation' }
SequenceableCollection class >> streamContents: blockWithArg limitedTo: sizeLimit [
	| stream |
	stream :=
		LimitedWriteStream
			on: (self streamSpecies new: (100 min: sizeLimit))
			limit: sizeLimit
			limitBlock: [^ stream contents].
	blockWithArg value: stream.
	^ stream contents
"
String streamContents: [:s | 1000 timesRepeat: [s nextPutAll: 'Junk']] limitedTo: 25
 'JunkJunkJunkJunkJunkJunkJ'
"
]

{ #category : #accessing }
SequenceableCollection class >> streamSpecies [
	"I return the class that is used for streaming. If override consider overriding #new:streamContents:"
	^ self
]

{ #category : #copying }
SequenceableCollection >> , otherCollection [ 
	"Concatenate two Strings or Collections."
	
	^ self copyReplaceFrom: self size + 1
		  to: self size
		  with: otherCollection
"
#(2 4 6 8) , #(who do we appreciate)
((2989 storeStringBase: 16) copyFrom: 4 to: 6) , ' boy!'
"
]

{ #category : #comparing }
SequenceableCollection >> = otherCollection [ 
	"Answer true if the receiver is equivalent to the otherCollection.
	First test for identity, then rule out different species and sizes of
	collections. As a last resort, examine each element of the receiver
	and the otherCollection."

	self == otherCollection ifTrue: [^ true].
	self species == otherCollection species ifFalse: [^ false].
	^ self hasEqualElements: otherCollection
]

{ #category : #converting }
SequenceableCollection >> @ aCollection [ 
	^ self with: aCollection collect: [:a :b | a @ b]
]

{ #category : #accessing }
SequenceableCollection >> after: target [
	"Answer the element after target.  Raise an error if target is not
	in the receiver, or if there are no elements after it."

	^ self after: target ifAbsent: [self errorNotFound: target]
]

{ #category : #accessing }
SequenceableCollection >> after: target ifAbsent: exceptionBlock [
	"Answer the element after target.  Answer the result of evaluation
	the exceptionBlock if target is not in the receiver, or if there are 
	no elements after it."

	| index |
	index := self indexOf: target.
	^ (index = 0 or: [index = self size])
		ifTrue: [exceptionBlock value]
		ifFalse: [self at: index + 1]
]

{ #category : #accessing }
SequenceableCollection >> allButFirst [
	"Answer a copy of the receiver containing all but the first
	element. Raise an error if there are not enough elements."

	^ self allButFirst: 1
]

{ #category : #accessing }
SequenceableCollection >> allButFirst: n [
	"Answer a copy of the receiver containing all but the first n
	elements. Raise an error if there are not enough elements."

	^ self copyFrom: n + 1 to: self size
]

{ #category : #enumerating }
SequenceableCollection >> allButFirstDo: block [

	2 to: self size do:
		[:index | block value: (self at: index)]
]

{ #category : #accessing }
SequenceableCollection >> allButLast [
	"Answer a copy of the receiver containing all but the last
	element. Raise an error if there are not enough elements."

	^ self allButLast: 1
]

{ #category : #accessing }
SequenceableCollection >> allButLast: n [
	"Answer a copy of the receiver containing all but the last n
	elements. Raise an error if there are not enough elements."

	^ self copyFrom: 1 to: self size - n
]

{ #category : #enumerating }
SequenceableCollection >> allButLastDo: block [

	1 to: self size - 1 do:
		[:index | block value: (self at: index)]
]

{ #category : #enumerating }
SequenceableCollection >> allPairsDo: aBinaryBlock [
	"
	Iterate over all the pairs of the receiver and apply the binaryBlock
	to each pair. 
	
	| res | 
	res := OrderedCollection new. 
	#(0 1 2 3) allPairsDo: [:first :second | res add: {first . second} ].
	res 
	 an OrderedCollection(#(0 0) #(0 1) #(0 2) #(0 3) #(1 0) #(1 1) #(1 2) #(1 3) #(2 0) #(2 1) #(2 2) #(2 3) #(3 0) #(3 1) #(3 2) #(3 3))
	"
	
	self do: [:first| self do: [:second| aBinaryBlock value: first value: second]]
]

{ #category : #accessing }
SequenceableCollection >> anyOne [
	"Answer a representative sample of the receiver. It raises an error when the collection is empty. This method can be helpful when needing to preinfer the nature of the contents of semi-homogeneous collections."
	"#(1 2 3) anyOne >>> 1"
	"
	([#() anyOne] on: SubscriptOutOfBounds do: [ :ex | 'whatever' ]) >>> 'whatever'
	"
	^ self first
]

{ #category : #splitjoin }
SequenceableCollection >> appendTo: aCollection [
	"double dispatch for join:"
	^ aCollection addAllLast: self
]

{ #category : #converting }
SequenceableCollection >> asArray [
	"Answer an Array whose elements are the elements of the receiver."

	^ Array withAll: self
]

{ #category : #converting }
SequenceableCollection >> asByteArray [
	"Answer a ByteArray whose elements are the elements of the receiver."

	^ ByteArray withAll: self
]

{ #category : #private }
SequenceableCollection >> asDigitsAt: anInteger in: aCollection do: aBlock [
	"(0 to: 1) asDigitsToPower: 4 do: [:each | Transcript cr; show: each printString]"

	self do: 
		[:each | 
		aCollection at: anInteger put: each.
		anInteger = aCollection size 
			ifTrue: [aBlock value: aCollection]
			ifFalse: [self asDigitsAt: anInteger + 1 in: aCollection do: aBlock]].
]

{ #category : #enumerating }
SequenceableCollection >> asDigitsToPower: anInteger do: aBlock [
	"Repeatedly value aBlock with a single Array.  Adjust the collection
	so that aBlock is presented all (self size raisedTo: anInteger) possible 
	combinations of the receiver's elements taken as digits of an anInteger long number."
	"(0 to: 1) asDigitsToPower: 4 do: [:each | Transcript cr; show: each printString]"

	| aCollection |
	aCollection := Array new: anInteger.
	self asDigitsAt: 1 in: aCollection do: aBlock
]

{ #category : #converting }
SequenceableCollection >> asFloatArray [
	"Answer a FloatArray whose elements are the elements of the receiver, in 
	the same order."

	| floatArray |
	floatArray := FloatArray new: self size.
	1 to: self size do:[:i| floatArray at: i put: (self at: i) asFloat ].
	^floatArray
]

{ #category : #converting }
SequenceableCollection >> asIntegerArray [
	"Answer an IntegerArray whose elements are the elements of the receiver, in 
	the same order."

	| intArray |
	intArray := IntegerArray new: self size.
	1 to: self size do:[:i| intArray at: i put: (self at: i)].
	^intArray
]

{ #category : #converting }
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

{ #category : #converting }
SequenceableCollection >> asWordArray [
	"Answer a WordArray whose elements are the elements of the receiver, in 
	the same order."

	| wordArray |
	wordArray := WordArray new: self size.
	1 to: self size do:[:i| wordArray at: i put: (self at: i)].
	^wordArray
]

{ #category : #accessing }
SequenceableCollection >> at: index ifAbsent: exceptionBlock [ 
	"Answer the element at my position index. If I do not contain an element 
	at index, answer the result of evaluating the argument, exceptionBlock."

	(index between: 1 and: self size) ifTrue: [^ self at: index].
	^ exceptionBlock value
]

{ #category : #accessing }
SequenceableCollection >> at: index incrementBy: value [
	^self at: index put: (self at: index) + value
]

{ #category : #accessing }
SequenceableCollection >> atAll: indexArray [
	"Answer a new collection like the receiver which contains all elements
	of the receiver at the indices of indexArray."
	"#('one' 'two' 'three' 'four') atAll: #(3 2 4)"

	| newCollection |
	newCollection := self species ofSize: indexArray size.
	1 to: indexArray size do:
		[:index |
		newCollection at: index put: (self at: (indexArray at: index))].
	^ newCollection
]

{ #category : #accessing }
SequenceableCollection >> atAll: aCollection put: anObject [ 
	"Put anObject at every index specified by the elements of aCollection."

	aCollection do: [:index | self at: index put: anObject].
	^ anObject
]

{ #category : #accessing }
SequenceableCollection >> atAll: indexArray putAll: valueArray [
	"Store the elements of valueArray into the slots
	of this collection selected by indexArray."

	indexArray with: valueArray do: [:index :value | self at: index put: value].
	^ valueArray
]

{ #category : #accessing }
SequenceableCollection >> atAllPut: anObject [ 
	"Put anObject at every one of the receiver's indices."

	| size |
	(size := self size) > 26 "first method faster from 27 accesses and on"
		ifTrue: [self from: 1 to: size put: anObject]
		ifFalse: [1 to: size do: [:index | self at: index put: anObject]]
]

{ #category : #accessing }
SequenceableCollection >> atLast: indexFromEnd [
	"Return element at indexFromEnd from the last position.
	 atLast: 1, returns the last element"

	^ self atLast: indexFromEnd ifAbsent: [self error: 'index out of range']
]

{ #category : #accessing }
SequenceableCollection >> atLast: indexFromEnd ifAbsent: block [
	"Return element at indexFromEnd from the last position.
	 atLast: 1 ifAbsent: [] returns the last element"

	^ self at: self size + 1 - indexFromEnd ifAbsent: block
]

{ #category : #accessing }
SequenceableCollection >> atLast: indexFromEnd put: obj [
	"Set the element at indexFromEnd from the last position.
	 atLast: 1 put: obj, sets the last element"

	^ self at: self size + 1 - indexFromEnd put: obj
]

{ #category : #accessing }
SequenceableCollection >> atPin: index [ 
	"Return the index'th element of me if possible.
	Return the first or last element if index is out of bounds."

	index < 1 ifTrue: [^ self first].
	index > self size ifTrue: [^ self last].
	^ self at: index
]

{ #category : #accessing }
SequenceableCollection >> atWrap: index [ 
	"Answer the index'th element of the receiver.  If index is out of bounds,
	let it wrap around from the end to the beginning until it is in bounds."

	^ self at: index - 1 \\ self size + 1
]

{ #category : #accessing }
SequenceableCollection >> atWrap: index put: value [
	"Store value into the index'th element of the receiver.  If index is out
	of bounds, let it wrap around from the end to the beginning until it 
	is in bounds. Answer value."

	^ self at: index  - 1 \\ self size + 1 put: value
]

{ #category : #accessing }
SequenceableCollection >> before: target [
	"Answer the receiver's element immediately before target. Raise an
	error if target is not an element of the receiver, or if there are no 
	elements before it (i.e. it is the first element)."

	^ self before: target ifAbsent: [self errorNotFound: target]
]

{ #category : #accessing }
SequenceableCollection >> before: target ifAbsent: exceptionBlock [
	"Answer the receiver's element immediately before target. Answer
	the result of evaluating the exceptionBlock if target is not an element
	of the receiver, or if there are no elements before it."

	| index |
	index := self indexOf: target.
	^ (index = 0 or: [index = 1])
		ifTrue: [exceptionBlock value]
		ifFalse: [self at: index - 1]
]

{ #category : #testing }
SequenceableCollection >> beginsWith: aSequenceableCollection [
	"Answer true if the receiver starts with the argument collection"
	(aSequenceableCollection isEmpty or: [self size < aSequenceableCollection size]) ifTrue: [^false].
	aSequenceableCollection withIndexDo: [:each :index | (self at: index) ~= each ifTrue: [^false]].
	^true
]

{ #category : #testing }
SequenceableCollection >> beginsWithAnyOf: aCollection [
	"Return true if the receiver starts with any of the elements in aCollection."
	^aCollection anySatisfy:[:prefix| self beginsWith: prefix]
]

{ #category : #enumerating }
SequenceableCollection >> collect: aBlock [ 
	"Evaluate aBlock with each of the receiver's elements as the argument.  
	Collect the resulting values into a collection like the receiver. Answer  
	the new collection."

	| newCollection |
	newCollection := self species new: self size.
	1 to: self size do:
		[:index |
		newCollection at: index put: (aBlock value: (self at: index))].
	^ newCollection
]

{ #category : #enumerating }
SequenceableCollection >> collect: aBlock from: firstIndex to: lastIndex [
	"Refer to the comment in Collection|collect:."

	| size result j |
	size := lastIndex - firstIndex + 1.
	result := self species new: size.
	j := firstIndex.
	1 to: size do: [:i | result at: i put: (aBlock value: (self at: j)). j := j + 1].
	^ result
]

{ #category : #enumerating }
SequenceableCollection >> collect: collectBlock thenReject: rejectBlock [
	"Optimized version of Collection>>#thenReject:"
	
	| each |
	
	^  self class new: self size streamContents: [ :stream |
		1 to: self size do: [:index |
			each := collectBlock value: (self at: index).
			(rejectBlock value: each)
				ifFalse: [ stream nextPut: each ]]]
]

{ #category : #enumerating }
SequenceableCollection >> collect: collectBlock thenSelect: selectBlock [
	"Optimized version of Collection>>#collect:thenSelect:"
	
	| each |
	
	^  self class new: self size streamContents: [ :stream |
		1 to: self size do: [:index |
			each := collectBlock value: (self at: index).
			(selectBlock value: each)
				ifTrue: [ stream nextPut: each ]]]
]

{ #category : #enumerating }
SequenceableCollection >> collectWithIndex: elementAndIndexBlock [
	"Use the new version with consistent naming"
	^ self withIndexCollect: elementAndIndexBlock
]

{ #category : #enumerating }
SequenceableCollection >> combinations [
	"Return all the combinations of elements of the receiver. Note that comabinations does not include the empty element contrary to the mathematical definition of combinations."

	"#(1 2 3 4) combinations 
	returns #(#(1) #(2) #(3) #(4) #(1 2) #(1 3) #(1 4) #(2 3) #(2 4) #(3 4) #(1 2 3) #(1 2 4) #(1 3 4) #(2 3 4) #(1 2 3 4))"

	^ Array
		streamContents: [ :stream | 
			1 to: self size do: [ :take | 
					self combinations: take atATimeDo: [ :combination | stream nextPut: combination copy ] ] ]
]

{ #category : #enumerating }
SequenceableCollection >> combinations: kk atATimeDo: aBlock [
	"Take the items in the receiver, kk at a time, and evaluate the block for each combination.  Hand in an array of elements of self as the block argument.  Each combination only occurs once, and order of the elements does not matter.  There are (self size take: kk) combinations."
	" 'abcde' combinations: 3 atATimeDo: [:each | Transcript cr; show: each printString]"

	| aCollection |
	aCollection := Array new: kk.
	self combinationsAt: 1 in: aCollection after: 0 do: aBlock
]

{ #category : #private }
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

{ #category : #converting }
SequenceableCollection >> concatenation [
	|result index|

	result := Array new: (self inject: 0 into: [:sum :each | sum + each size]).
	index := 0.
	self do: [:each | each do: [:item | result at: (index := index+1) put: item]].
	^result
]

{ #category : #copying }
SequenceableCollection >> copyAfter: anElement [
	"Answer a copy of the receiver from after the first occurence
	of anElement up to the end. If no such element exists, answer 
	an empty copy."

	^ self allButFirst: (self indexOf: anElement ifAbsent: [^ self copyEmpty])
]

{ #category : #copying }
SequenceableCollection >> copyAfterLast: anElement [
	"Answer a copy of the receiver from after the last occurence
	of anElement up to the end. If no such element exists, answer 
	an empty copy."

	^ self allButFirst: (self lastIndexOf: anElement ifAbsent: [^ self copyEmpty])
]

{ #category : #copying }
SequenceableCollection >> copyEmpty [
	^ self species new: 0
]

{ #category : #copying }
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

{ #category : #copying }
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

{ #category : #copying }
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

{ #category : #copying }
SequenceableCollection >> copyUpThrough: anElement [
    "Answer all elements up to and including anObject. If there
     is no such object, answer a copy of the receiver."

	^self first: (self indexOf: anElement ifAbsent: [^ self copy])
]

{ #category : #copying }
SequenceableCollection >> copyUpTo: anElement [ 
	"Answer all elements up to but not including anObject. If there
	is no such object, answer a copy of the receiver."

	^ self first: (self indexOf: anElement ifAbsent: [^ self copy]) - 1
]

{ #category : #copying }
SequenceableCollection >> copyUpToLast: anElement [
	"Answer a copy of the receiver from index 1 to the last occurrence of 
	anElement, not including anElement."

	^ self first: (self lastIndexOf: anElement ifAbsent: [^ self copy]) - 1
]

{ #category : #copying }
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

{ #category : #copying }
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

{ #category : #copying }
SequenceableCollection >> copyWithoutFirst [
	"Deprecatd. Return a copy of the receiver which doesn't include
	the first element."

	^ self allButFirst
]

{ #category : #copying }
SequenceableCollection >> copyWithoutIndex: index [
	"Return a copy containing all elements except the index-th."

	| copy |
	copy := self species ofSize: self size - 1.
	copy replaceFrom: 1 to: index-1 with: self startingAt: 1.
	copy replaceFrom: index to: copy size with: self startingAt: index+1.
	^ copy
]

{ #category : #enumerating }
SequenceableCollection >> detectIndex: aBlock [

	^ self detectIndex: aBlock ifNone: [ self errorNotFound: aBlock ] 
]

{ #category : #enumerating }
SequenceableCollection >> detectIndex: aBlock ifNone: exceptionBlock [

	self doWithIndex: [:each :index | (aBlock value: each) ifTrue: [^ index]].
	^ exceptionBlock value
]

{ #category : #enumerating }
SequenceableCollection >> do: aBlock [ 
	"Refer to the comment in Collection|do:."
	1 to: self size do:
		[:index | aBlock value: (self at: index)]
]

{ #category : #enumerating }
SequenceableCollection >> do: elementBlock separatedBy: separatorBlock [
	"Evaluate the elementBlock for all elements in the receiver,
	and evaluate the separatorBlock between."

	1 to: self size do:
		[:index |
		index = 1 ifFalse: [separatorBlock value].
		elementBlock value: (self at: index)]
]

{ #category : #enumerating }
SequenceableCollection >> do: aBlock without: anItem [
	"Enumerate all elements in the receiver.
	Execute aBlock for those elements that are not equal to the given item"
	"Refer to the comment in Collection|do:."
	1 to: self size do:
		[:index | anItem = (self at: index) ifFalse:[aBlock value: (self at: index)]]
]

{ #category : #enumerating }
SequenceableCollection >> doWithIndex: elementAndIndexBlock [
	"Just like with:do: except that the iteration index supplies the second argument to the block. 
	#(11 22 13) doWithIndex: [ :each :i | Transcript show: (each * each + i) ;cr ].
	prints: 
		122
		486
		172

	Use the new version with consistent naming"
	^ self withIndexDo: elementAndIndexBlock
]

{ #category : #accessing }
SequenceableCollection >> eighth [
	"Answer the eighth element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 8
]

{ #category : #testing }
SequenceableCollection >> endsWith: aSequenceableCollection [
	"Answer true if the receiver ends with the argument collection"
	
	| start |
	(aSequenceableCollection isEmpty or: [self size < aSequenceableCollection size]) ifTrue: [^false].
	start := self size - aSequenceableCollection size.
	aSequenceableCollection withIndexDo: [:each :index | (self at: start + index) ~= each ifTrue: [^false]].
	^true
]

{ #category : #testing }
SequenceableCollection >> endsWithAnyOf: aCollection [
	"Return true if the receiver ends with any of the elements in aCollection."
	^aCollection anySatisfy:[:suffix| self endsWith: suffix]
]

{ #category : #private }
SequenceableCollection >> errorOutOfBounds [

	SubscriptOutOfBounds signal
]

{ #category : #accessing }
SequenceableCollection >> fifth [
	"Answer the fifth element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 5
]

{ #category : #enumerating }
SequenceableCollection >> findBinary: aBlock [
	"Search for an element in the receiver using binary search.
	The argument aBlock is a one-element block returning
		0 	- if the element is the one searched for
		<0	- if the search should continue in the first half
		>0	- if the search should continue in the second half
	If no matching element is found, raise an error.
	Examples:
		#(1 3 5 7 11 15 23) findBinary: [ :arg | 11 - arg ]. "
		
	^ self 
		findBinary: aBlock 
		do: [ :found | found ] 
		ifNone: [ self errorNotFound: aBlock ]
]

{ #category : #enumerating }
SequenceableCollection >> findBinary: aBlock do: actionBlock ifNone: exceptionBlock [
	"Search for an element in the receiver using binary search.
	The argument aBlock is a one-element block returning
		0 	- if the element is the one searched for
		<0	- if the search should continue in the first half
		>0	- if the search should continue in the second half
	If found, evaluate actionBlock with the found element as argument
	If no matching element is found, evaluate exceptionBlock,
	with the 'bounding' elements or nil as arguments.
	Examples:
		#(1 3 5 7 11 15 23)
			findBinary: [ :arg | 11 - arg ]
			do: [ :found | found ]
			ifNone: [ :a :b | 'between: ', {a. b} printString ]
		#(1 3 5 7 11 15 23)
			findBinary: [ :arg | 12 - arg ]
			do: [ :found | found ]
			ifNone: [ :a :b | 'between: ', {a. b} printString ]
		#(1 3 5 7 11 15 23)
			findBinary: [ :arg | 0.5 - arg ]
			do: [ :found | found ]
			ifNone: [ :a :b | 'between: ', {a. b} printString ]
		#(1 3 5 7 11 15 23)
			findBinary: [ :arg | 25 - arg ]
			do: [ :found | found ]
			ifNone: [ :a :b | 'between: ',{a. b} printString ] "
	
	^ self
		findBinaryIndex: aBlock
		do: [ :foundIndex |
			actionBlock value: (self at: foundIndex) ]
		ifNone: [ :prevIndex :nextIndex |
			exceptionBlock
				cull: (prevIndex > 0 ifTrue: [ self at: prevIndex ])
				cull: (nextIndex <= self size ifTrue: [ self at: nextIndex ]) ]

]

{ #category : #enumerating }
SequenceableCollection >> findBinary: aBlock ifNone: exceptionBlock [
	"Search for an element in the receiver using binary search.
	The argument aBlock is a one-element block returning
		0 	- if the element is the one searched for
		<0	- if the search should continue in the first half
		>0	- if the search should continue in the second half
	If no matching element is found, evaluate exceptionBlock,
	with the 'bounding' elements as optional arguments."
	
	^ self 
		findBinary: aBlock 
		do: [ :found | found ] 
		ifNone: exceptionBlock
]

{ #category : #enumerating }
SequenceableCollection >> findBinaryIndex: aBlock [
	"Search for an element in the receiver using binary search.
	The argument aBlock is a one-element block returning
		0 	- if the element is the one searched for
		<0	- if the search should continue in the first half
		>0	- if the search should continue in the second half
	If no matching element is found, raise an error.
	Examples:
		#(1 3 5 7 11 15 23) findBinaryIndex: [ :arg | 11 - arg ]
	"
	^ self 
		findBinaryIndex: aBlock 
		do: [ :found | found ] 
		ifNone: [ self errorNotFound: aBlock ]
]

{ #category : #enumerating }
SequenceableCollection >> findBinaryIndex: aBlock do: actionBlock ifNone: exceptionBlock [
	"Search for an element in the receiver using binary search.
	The argument aBlock is a one-element block returning
		0 	- if the element is the one searched for
		<0	- if the search should continue in the first half
		>0	- if the search should continue in the second half
	If found, evaluate actionBlock with the index as argument
	If no matching element is found, evaluate exceptionBlock,
	with the indexes of the 'bounding' elements as arguments.
	Warning: Might give invalid indexes, see examples below
	Examples:
		#(1 3 5 7 11 15 23)
			findBinaryIndex: [ :arg | 11 - arg ]
			do: [ :found | found ]
			ifNone: [ :a :b | 'between: ', {a. b} printString ]
		#(1 3 5 7 11 15 23)
			findBinaryIndex: [ :arg | 12 - arg ]
			do: [ :found | found ]
			ifNone: [ :a :b | 'between: ', {a. b} printString ]
		#(1 3 5 7 11 15 23)
			findBinaryIndex: [ :arg | 0.5 - arg ]
			do: [ :found | found ]
			ifNone: [ :a :b | 'between: ', {a. b} printString ]
		#(1 3 5 7 11 15 23)
			findBinaryIndex: [ :arg | 25 - arg ]
			do: [ :found | found ]
			ifNone: [ :a :b | 'between: ',{a. b} printString ] "
			
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

{ #category : #enumerating }
SequenceableCollection >> findBinaryIndex: aBlock ifNone: exceptionBlock [
	"Search for an element in the receiver using binary search.
	The argument aBlock is a one-element block returning
		0 	- if the element is the one searched for
		<0	- if the search should continue in the first half
		>0	- if the search should continue in the second half
	If no matching element is found, evaluate exceptionBlock,
	with the 'bounding' elements as optional arguments."

	^ self 
		findBinaryIndex: aBlock 
		do: [ :found | found ] 
		ifNone: exceptionBlock
]

{ #category : #enumerating }
SequenceableCollection >> findFirst: aBlock [
	"Return the index of my first element for which aBlock evaluates as true."

	| index |
	index := 0.
	[(index := index + 1) <= self size] whileTrue:
		[(aBlock value: (self at: index)) ifTrue: [^index]].
	^ 0
]

{ #category : #enumerating }
SequenceableCollection >> findLast: aBlock [
	"Return the index of my last element for which aBlock evaluates as true."

	| index |
	index := self size + 1.
	[(index := index - 1) >= 1] whileTrue:
		[(aBlock value: (self at: index)) ifTrue: [^index]].
	^ 0
]

{ #category : #accessing }
SequenceableCollection >> first [
	"Answer the first element of the receiver"

	^ self at: 1
]

{ #category : #accessing }
SequenceableCollection >> first: n [
	"Answer the first n elements of the receiver.
	Raise an error if there are not enough elements."

	^ self copyFrom: 1 to: n
]

{ #category : #enumerating }
SequenceableCollection >> flatCollect: aBlock [ 
	"Evaluate aBlock for each of the receiver's elements and answer the
	list of all resulting values flatten one level. Assumes that aBlock returns some kind
	of collection for each element. optimized version for Sequencable Collection and subclasses
	implementing #writeStream"
	"
	#( (2 -3) (4 -5) #(-6)) flatCollect: [ :e | e abs  ]  
	>>> #(2 3 4 5 6)
	"
	"
	#( (2 -3) #((4 -5)) #(-6)) flatCollect: [ :e | e abs  ]  
	>>> #(2 3 #(4 5) 6)
	"
	
	| stream |
	self isEmpty ifTrue: [ ^ self copy ].
	stream := (self species new: 0) writeStream.
	self do: [ :each | stream nextPutAll: (aBlock value: each) ].
	^ stream contents
]

{ #category : #copying }
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

{ #category : #copying }
SequenceableCollection >> forceTo: length paddingWith: elem [
	"Force the length of the collection to length, padding
	if necessary with elem.  Note that this makes a copy."

	| newCollection |
	newCollection := self species new: length withAll: elem.
	newCollection replaceFrom: 1 to: (self size min: length) with: self startingAt: 1.
	^ newCollection
]

{ #category : #accessing }
SequenceableCollection >> fourth [
	"Answer the fourth element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 4
]

{ #category : #enumerating }
SequenceableCollection >> from: start to: stop do: aBlock [
	"Evaluate aBlock for all elements between start and stop (inclusive)."

	start to: stop do: [:index | aBlock value: (self at: index)]
]

{ #category : #accessing }
SequenceableCollection >> from: startIndex to: endIndex put: anObject [
	"Put anObject in all indexes between startIndex 
	and endIndex. Very fast. Faster than to:do: for
	more than 26 positions. Answer anObject"

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

{ #category : #enumerating }
SequenceableCollection >> groupsOf: n atATimeCollect: aBlock [ 
	"Evaluate aBlock with my elements taken n at a time. Ignore any leftovers at the end. 
	Allows use of a flattened array for things that naturally group into groups of n. 
	If aBlock has a single argument, pass it an array of n items, otherwise, pass the items as separate arguments. See also pairsDo:"
	
	"
	#(16 17 17 16 18 17 18   19 19 19 18 19 19 20   19 20 19 20 20 20 19   20) groupsOf: 7 atATimeCollect: [ :x | x ]
	>>> #(#(16 17 17 16 18 17 18) #(19 19 19 18 19 19 20) #(19 20 19 20 20 20 19))
	"
	"
	#(1 1 1 10 10 10 100  100 100) 
		groupsOf: 3 atATimeCollect: [ :x :y :z | x + y + z] 
	>>> #(3 30 300)
	"
	
	"
	#(1 1 1 10 10 10 100  100 100) 
		groupsOf: 3 atATimeCollect: [ :x | x ]  
	>>> #(#(1 1 1) #(10 10 10) #(100 100 100))
	"
	
	| passArray |
	passArray := aBlock numArgs = 1.
	^(n
		to: self size
		by: n)
		collect: [:index | | args | 
			args := (self copyFrom: index - n + 1 to: index) asArray.
			passArray
				ifTrue: [aBlock value: args]
				ifFalse: [aBlock valueWithArguments: args]]
]

{ #category : #enumerating }
SequenceableCollection >> groupsOf: n atATimeDo: aBlock [ 
	"Evaluate aBlock with my elements taken n at a time. Ignore any leftovers at the end.
	Allows use of a flattened 
	array for things that naturally group into groups of n.
	If aBlock has a single argument, pass it an array of n items,
	otherwise, pass the items as separate arguments.
	See also pairsDo:"
	| passArray |
	passArray := (aBlock numArgs = 1).
	n
		to: self size
		by: n
		do: [:index | | args | 
			args := (self copyFrom: index - n + 1 to: index) asArray.
			passArray ifTrue: [ aBlock value: args ]
				ifFalse: [ aBlock valueWithArguments: args ]].
]

{ #category : #copying }
SequenceableCollection >> grownBy: length [
	"Answer a copy of receiver collection with size grown by length"

	| newCollection size |
	size := self size.
	newCollection := self species ofSize: size + length.
	newCollection replaceFrom: 1 to: size with: self startingAt: 1.
	^ newCollection
]

{ #category : #comparing }
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

{ #category : #comparing }
SequenceableCollection >> hash [
	| hash |

	hash := self species hash.
	1 to: self size do: [:i | hash := (hash + (self at: i) hash) hashMultiply].
	^hash
]

{ #category : #accessing }
SequenceableCollection >> identityIndexOf: anElement [ 
	"Answer the index of anElement within the receiver. If the receiver does 
	not contain anElement, answer 0."

	^self identityIndexOf: anElement ifAbsent: [0]
]

{ #category : #accessing }
SequenceableCollection >> identityIndexOf: anElement ifAbsent: exceptionBlock [
	"Answer the index of anElement within the receiver. If the receiver does 
	not contain anElement, answer the result of evaluating the argument, 
	exceptionBlock."
	1 to: self size do:
		[:i | (self at: i) == anElement ifTrue: [^ i]].
	^ exceptionBlock value
]

{ #category : #testing }
SequenceableCollection >> includes: anObject [
	"Answer whether anObject is one of the receiver's elements."

	^ (self indexOf: anObject) ~= 0
]

{ #category : #accessing }
SequenceableCollection >> indexOf: anElement [
	"Answer the index of the first occurrence of anElement within the  
	receiver. If the receiver does not contain anElement, answer 0."

	^ self indexOf: anElement ifAbsent: 0
]

{ #category : #accessing }
SequenceableCollection >> indexOf: anElement ifAbsent: exceptionBlock [
	"Answer the index of the first occurence of anElement within the  
	receiver. If the receiver does not contain anElement, answer the 
	result of evaluating the argument, exceptionBlock."

	^ self indexOf: anElement startingAt: 1 ifAbsent: exceptionBlock
]

{ #category : #accessing }
SequenceableCollection >> indexOf: anElement startingAt: start [
       "Answer the index of the first occurence of anElement after start within the receiver. If the receiver does not contain anElement, answer 0."

	^self indexOf: anElement startingAt: start ifAbsent: 0
]

{ #category : #accessing }
SequenceableCollection >> indexOf: anElement startingAt: start ifAbsent: exceptionBlock [
	"Answer the index of the first occurence of anElement after start
	within the receiver. If the receiver does not contain anElement, 
	answer the 	result of evaluating the argument, exceptionBlock."

	start to: self size do:
		[:index |
		(self at: index) = anElement ifTrue: [^ index]].
	^ exceptionBlock value
]

{ #category : #accessing }
SequenceableCollection >> indexOfAnyOf: aCollection [
	"Answer the index of the first occurence of any element included in aCollection within the receiver.
	If the receiver does not contain anElement, answer zero, which is an invalid index."

	^self indexOfAnyOf: aCollection startingAt: 1 ifAbsent: [0]
]

{ #category : #accessing }
SequenceableCollection >> indexOfAnyOf: aCollection ifAbsent: exceptionBlock [
	"Answer the index of the first occurence of any element included in aCollection within the receiver.
	If the receiver does not contain anElement, answer the result of evaluating the argument, exceptionBlock."

	^self indexOfAnyOf: aCollection startingAt: 1 ifAbsent: exceptionBlock
]

{ #category : #accessing }
SequenceableCollection >> indexOfAnyOf: aCollection startingAt: start [
	"Answer the index of the first occurence of any element included in aCollection after start within the receiver.
	If the receiver does not contain anElement, answer zero, which is an invalid index."

	^self indexOfAnyOf: aCollection startingAt: start ifAbsent: [0]
]

{ #category : #accessing }
SequenceableCollection >> indexOfAnyOf: aCollection startingAt: start ifAbsent: exceptionBlock [
	"Answer the index of the first occurence of any element included in aCollection after start within the receiver.
	If the receiver does not contain anElement, answer the result of evaluating the argument, exceptionBlock.
	Note: it is user responsibility to provide aCollection that behaves relatevily fast when asked for includes: (like a Set)"

	start to: self size do:
		[:index |
		(aCollection includes: (self at: index)) ifTrue: [^ index]].
	^ exceptionBlock value
]

{ #category : #accessing }
SequenceableCollection >> indexOfSubCollection: aSubCollection startingAt: anIndex [ 
	"Answer the index of the receiver's first element, such that that element 
	equals the first element of aSubCollection, and the next elements equal 
	the rest of the elements of aSubCollection. Begin the search at element 
	anIndex of the receiver. If no such match is found, answer 0."

	^self
		indexOfSubCollection: aSubCollection
		startingAt: anIndex
		ifAbsent: [0]
]

{ #category : #accessing }
SequenceableCollection >> indexOfSubCollection: sub startingAt: start ifAbsent: exceptionBlock [
	"Answer the index of the receiver's first element, such that that element 
	equals the first element of sub, and the next elements equal 
	the rest of the elements of sub. Begin the search at element 
	start of the receiver. If no such match is found, answer the result of 
	evaluating argument, exceptionBlock."
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

{ #category : #accessing }
SequenceableCollection >> integerAt: index [
	"Return the integer at the given index"
	^self at: index
]

{ #category : #accessing }
SequenceableCollection >> integerAt: index put: value [
	"Return the integer at the given index"
	^self at: index put: value
]

{ #category : #converting }
SequenceableCollection >> isSequenceable [
	^ true
]

{ #category : #sorting }
SequenceableCollection >> isSorted [
	"Return true if the receiver is sorted by the given criterion.
	Optimization for isSortedBy: [:a :b | a <= b]."

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

{ #category : #sorting }
SequenceableCollection >> isSortedBy: aBlock [
	"Return true if the receiver is sorted by the given criterion."

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

{ #category : #splitjoin }
SequenceableCollection >> join: aCollection [
	"Append the elements of the argument, aSequenceableCollection, separating them by the receiver."
	
	"({Character space. Character space} join: #('Pharo' 'is' 'cool')) >>>  {$P. $h. $a. $r. $o. Character space. Character space. $i. $s. Character space. Character space. $c. $o. $o. $l}"

	"NB: this implementation only works for Array, since WriteStreams only work for Arrays and Strings. (!) Overridden in OrderedCollection and SortedCollection."
	
	^ self class
		streamContents: [:stream | aCollection
				do: [:each | each joinTo: stream]
				separatedBy: [stream nextPutAll: self]]
]

{ #category : #splitjoin }
SequenceableCollection >> joinTo: stream [
	"double dispatch for join:"
	^ stream nextPutAll: self
]

{ #category : #splitjoin }
SequenceableCollection >> joinUsing: joiner [
	"Append the elements of the receiver separating them with the joiner - character, string or sequenceable collection. Return collection of the same collection class as 'joiner', or a String"
	^ joiner join: self
]

{ #category : #splitjoin }
SequenceableCollection >> joinUsing: joiner last: last [ 
	"Append the elements of the receiver separating them with the joiner argument and handle the last elment separation using the last argument."
	
	"(#(1 2 3 4) joinUsing: ', ' last: ' and ') >>> '1, 2, 3 and 4'"
	
	(self size = 0) ifTrue: [  ^ '' ].
	(self size = 1) ifTrue: [  ^ self first asString ].
	^ last join: (Array
				with: (joiner join: self allButLast)
				with: self last)
]

{ #category : #enumerating }
SequenceableCollection >> keysAndValuesDo: aBlock [ 
	"Enumerate the receiver with all the keys (aka indices) and values."

	1 to: self size do: [:index | aBlock value: index value: (self at: index)]
]

{ #category : #accessing }
SequenceableCollection >> last [
	"Answer the last element of the receiver"

	^ self at: self size
]

{ #category : #accessing }
SequenceableCollection >> last: n [
	"Answer the last n elements of the receiver.  
	Raise an error if there are not enough elements."

	| size |
	size := self size.
	^ self copyFrom: size - n + 1 to: size
]

{ #category : #accessing }
SequenceableCollection >> lastIndexOf: anElement [
	"Answer the index of the last occurence of anElement within the 
	receiver. If the receiver does not contain anElement, answer 0."

	^ self lastIndexOf: anElement startingAt: self size ifAbsent: [0]
]

{ #category : #accessing }
SequenceableCollection >> lastIndexOf: anElement ifAbsent: exceptionBlock [
	"Answer the index of the last occurence of anElement within the  
	receiver. If the receiver does not contain anElement, answer the
	result of evaluating the argument, exceptionBlock."
	^self lastIndexOf: anElement startingAt: self size ifAbsent: exceptionBlock
]

{ #category : #accessing }
SequenceableCollection >> lastIndexOf: anElement startingAt: lastIndex ifAbsent: exceptionBlock [
	"Answer the index of the last occurence of anElement within the  
	receiver. If the receiver does not contain anElement, answer the
	result of evaluating the argument, exceptionBlock."

	lastIndex to: 1 by: -1 do:
		[:index |
		(self at: index) = anElement ifTrue: [^ index]].
	^ exceptionBlock value
]

{ #category : #accessing }
SequenceableCollection >> lastIndexOfAnyOf: aCollection startingAt: lastIndex ifAbsent: exceptionBlock [
	"Answer the index of the last occurence of anElement within the  
	receiver. If the receiver does not contain anElement, answer the
	result of evaluating the argument, exceptionBlock."

	lastIndex to: 1 by: -1 do:
		[:index |
		(aCollection includes: (self at: index)) ifTrue: [^ index]].
	^ exceptionBlock value
]

{ #category : #sorting }
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

{ #category : #sorting }
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

{ #category : #sorting }
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

{ #category : #accessing }
SequenceableCollection >> middle [
	"Answer the middle element of the receiver."
	^ self at: self size // 2 + 1
]

{ #category : #enumerating }
SequenceableCollection >> nextToLast [
	^self at: self size - 1
]

{ #category : #accessing }
SequenceableCollection >> ninth [
	"Answer the ninth element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 9
]

{ #category : #enumerating }
SequenceableCollection >> overlappingPairsCollect: aBlock [ 
	"Answer the result of evaluating aBlock with all of the overlapping pairs of my elements."
	| retval |
	retval := self species ofSize: self size - 1.
	1 to: self size - 1
		do: [:i | retval at: i put: (aBlock value: (self at: i) value: (self at: i + 1)) ].
	^retval
]

{ #category : #enumerating }
SequenceableCollection >> overlappingPairsDo: aBlock [ 
	"Emit overlapping pairs of my elements into aBlock"

	1 to: self size - 1
		do: [:i | aBlock value: (self at: i) value: (self at: i + 1)]
]

{ #category : #enumerating }
SequenceableCollection >> overlappingPairsWithIndexDo: aBlock [ 
	"Emit overlapping pairs of my elements into aBlock, along with an index."

	1 to: self size - 1
		do: [:i | aBlock value: (self at: i) value: (self at: i + 1) value: i ]
]

{ #category : #enumerating }
SequenceableCollection >> paddedWith: otherCollection do: twoArgBlock [ 
	"Evaluate twoArgBlock with corresponding elements from this collection and otherCollection.
	Missing elements from either will be passed as nil."
	1 to: (self size max: otherCollection size) do:
		[:index | twoArgBlock value: (self at: index ifAbsent: [])
				value: (otherCollection at: index ifAbsent: [])]
]

{ #category : #enumerating }
SequenceableCollection >> pairsCollect: aBlock [ 
	"Evaluate aBlock with my elements taken two at a time, and return an Array with the results"

	^ (1 to: self size // 2) collect:
		[:index | aBlock value: (self at: 2 * index - 1) value: (self at: 2 * index)]
"
#(1 'fred' 2 'charlie' 3 'elmer') pairsCollect:
	[:a :b | b, ' is number ', a printString]
"
]

{ #category : #enumerating }
SequenceableCollection >> pairsDo: aBlock [ 
	"Evaluate aBlock with my elements taken two at a time.  If there's an odd number of items, ignore the last one.  Allows use of a flattened array for things that naturally group into pairs.  See also pairsCollect:"

	1 to: self size // 2 do:
		[:index | aBlock value: (self at: 2 * index - 1) value: (self at: 2 * index)]
"
#(1 'fred' 2 'charlie' 3 'elmer') pairsDo:
	[:a :b | Transcript cr; show: b, ' is number ', a printString]
"
]

{ #category : #enumerating }
SequenceableCollection >> permutationsDo: aBlock [
	"Repeatly value aBlock with a single copy of the receiver. Reorder the copy
	so that aBlock is presented all (self size factorial) possible permutations."
	"(1 to: 4) permutationsDo: [:each | Transcript cr; show: each printString]"

	self shallowCopy permutationsStartingAt: 1 do: aBlock
]

{ #category : #private }
SequenceableCollection >> permutationsStartingAt: anInteger do: aBlock [
	"#(1 2 3 4) permutationsDo: [:each | Transcript cr; show: each printString]"

	anInteger > self size ifTrue: [^self].
	anInteger = self size ifTrue: [^aBlock value: self].
	anInteger to: self size do:
		[:i | self swap: anInteger with: i.
		self permutationsStartingAt: anInteger + 1 do: aBlock.
		self swap: anInteger with: i]
]

{ #category : #enumerating }
SequenceableCollection >> polynomialEval: thisX [
	| sum valToPower |
	"Treat myself as the coeficients of a polynomial in X.  Evaluate it with thisX.  First element is the constant and last is the coeficient for the highest power."
	"  #(1 2 3) polynomialEval: 2   "   "is 3*X^2 + 2*X + 1 with X = 2"

	sum := self first.
	valToPower := thisX.
	2 to: self size do: [:ind | 
		sum := sum + ((self at: ind) * valToPower).
		valToPower := valToPower * thisX].
	^ sum
]

{ #category : #streaming }
SequenceableCollection >> putOn: aStream [
	self do: [ :each | each putOn: aStream ]
]

{ #category : #converting }
SequenceableCollection >> readStream [
	^ ReadStream on: self
]

{ #category : #streaming }
SequenceableCollection >> readStreamDo: aBlock [
	"Evaluates the argument with the read stream of the collection. Answers the result."
	
	"#(3 4 5) readStreamDo: [ :stream | stream contents ] >>> #(3 4 5)"

	^ aBlock value: self readStream
]

{ #category : #enumerating }
SequenceableCollection >> reduce: aBlock [
	"Fold the result of the receiver into aBlock. The argument aBlock must take two or more arguments. It applies the argument, binaryBlock cumulatively to the elements of the receiver. For sequenceable collections the elements will be used in order, for unordered collections the order is unspecified."
	
	"#(1 2 3) reduce: [ :a :b | a + b ]
		--> 1 + 2 + 3 = 6
	#(1 2 3 4 5) reduce: [ :a :b :c | a + b + c ]
		--> 1 + 2 + 3 + 4 + 5 = 15"
		
	^ self reduceLeft: aBlock
]

{ #category : #enumerating }
SequenceableCollection >> reduceLeft: aBlock [
	"Fold the result of the receiver from left to right into aBlock. The argument aBlock must take two or more arguments."

	"#(1 2 3) reduceLeft: [ :a :b | a - b ].
		--> ((1 - 2) - 3) = -4
	#(1 + 3 - 5) reduceLeft: [ :a :op :b | a perform: op with: b ].
		--> ((1 + 3) - 5) = -1"

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

{ #category : #enumerating }
SequenceableCollection >> reduceRight: aBlock [
	"Fold the result of the receiver from right to left into aBlock. The argument aBlock must take two or more arguments."

	"#(1 2 3) reduceRight: [ :a :b | a - b ].
		--> (1 - (2 - 3)) = 2
	#(1 + 3 - 5) reduceRight: [ :a :op :b | a perform: op with: b ].
		--> (1 + (3 - 5)) = -1"

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

{ #category : #enumerating }
SequenceableCollection >> reject: rejectBlock [ 
	"Optimized version of Collection>>#reject:"
	
	| each |
	
	^ self class new: self size streamContents: [ :stream|
		1 to: self size do: [ :index |
			(rejectBlock value: (each := self at: index))
				ifFalse: [ stream nextPut: each ]]]
]

{ #category : #enumerating }
SequenceableCollection >> reject: rejectBlock thenCollect: collectBlock [
	"Optimized version of Collection>>#reject:thenCollect:"
	
	| each |

	^ self class new: self size streamContents: [ :stream|
		1 to: self size do: [ :index |
			(rejectBlock value: (each := self at: index))
				ifFalse: [ stream nextPut: (collectBlock value: each) ]]]
]

{ #category : #enumerating }
SequenceableCollection >> reject: rejectBlock thenDo: aBlock [
	"Refer to the comment in Collection>>#reject:thenDo:"
	
	|  each |
	
	1 to: self size do: [ :index |
		(rejectBlock value: (each := self at: index))
			ifFalse: [ aBlock value: each ]].
]

{ #category : #removing }
SequenceableCollection >> remove: oldObject ifAbsent: anExceptionBlock [ 
	"SequencableCollections cannot implement removing."

	self shouldNotImplement
]

{ #category : #enumerating }
SequenceableCollection >> replace: aBlock [ 
	"Evaluate aBlock with each of the receiver's elements as the argument.  
	Collect the resulting values into self."

	1 to: self size do: [ :index |
		self at: index put: (aBlock value: (self at: index)) ]
]

{ #category : #accessing }
SequenceableCollection >> replaceAll: oldObject with: newObject [ 
	"Replace all occurences of oldObject with newObject"
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

{ #category : #accessing }
SequenceableCollection >> replaceFrom: start to: stop with: replacement [ 
	"This destructively replaces elements from start to stop in the receiver. 
	Answer the receiver itself. Use copyReplaceFrom:to:with: for 
	insertion/deletion which may alter the size of the result."

	replacement size = (stop - start + 1)
		ifFalse: [self error: 'Size of replacement doesnt match'].
	^self replaceFrom: start to: stop with: replacement startingAt: 1
]

{ #category : #accessing }
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

{ #category : #converting }
SequenceableCollection >> reverse [
	"Answer a copy of the receiver with element order reversed, as expected by ANSI."

	^ self reversed
]

{ #category : #enumerating }
SequenceableCollection >> reverseDo: aBlock [
	"Evaluate aBlock with each of the receiver's elements as the argument, 
	starting with the last element and taking each in sequence up to the 
	first. For SequenceableCollections, this is the reverse of the enumeration 
	for do:."

	self size to: 1 by: -1 do: [:index | aBlock value: (self at: index)]
]

{ #category : #enumerating }
SequenceableCollection >> reverseWith: aSequenceableCollection do: aBlock [ 
	"Evaluate aBlock with each of the receiver's elements, in reverse order, 
	along with the  corresponding element, also in reverse order, from 
	aSequencableCollection. "

	self size ~= aSequenceableCollection size ifTrue: [^ self errorSizeMismatch].
	self size to: 1 by: -1 do: [:index | 
			aBlock 
				value: (self at: index)
				value: (aSequenceableCollection at: index)]
]

{ #category : #enumerating }
SequenceableCollection >> reverseWithIndexDo: elementAndIndexBlock [ 
	"Just like reverseWith:do: except that the iteration index supplies the second argument to the block."
	self size to: 1 by: -1 do: [:index |
		elementAndIndexBlock
			value: (self at: index)
			value: index]
]

{ #category : #converting }
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

{ #category : #accessing }
SequenceableCollection >> second [
	"Answer the second element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 2
]

{ #category : #enumerating }
SequenceableCollection >> select: aBlock [ 
	"Optimized version of Collection>>#select:"
	
	| each |
	^ self class new: self size streamContents: [ :stream|
		1 to: self size do: [ :index |
			(aBlock value: (each := self at: index))
				ifTrue: [ stream nextPut: each ]]]
]

{ #category : #enumerating }
SequenceableCollection >> select: selectBlock thenCollect: collectBlock [
	"Optimized version of Collection>>#select:thenCollect:"
	
	| each |
	^ self class new: self size streamContents: [ :stream|
		1 to: self size do: [ :index |
			(selectBlock value: (each := self at: index))
				ifTrue: [ stream nextPut: (collectBlock value: each) ]]]
]

{ #category : #enumerating }
SequenceableCollection >> select: selectBock thenDo: aBlock [
	"Refer to the comment in Collection>>#select:thenDo:"
	
	| each |
	1 to: self size do: [ :index |
		(selectBock value: (each := self at: index))
			ifTrue: [ aBlock value: each ]].
]

{ #category : #accessing }
SequenceableCollection >> seventh [
	"Answer the seventh element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 7
]

{ #category : #shuffling }
SequenceableCollection >> shuffle [
	^ self shuffleBy: Random new
]

{ #category : #shuffling }
SequenceableCollection >> shuffleBy: aRandom [
	"Durstenfeld's version of the Fisher-Yates shuffle"

	self size to: 2 by: -1 do: [ :i | 
		self swap: i with: (aRandom nextInt: i) ]
]

{ #category : #copying }
SequenceableCollection >> shuffled [
	^ self copy shuffle
]

{ #category : #accessing }
SequenceableCollection >> sixth [
	"Answer the sixth element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 6
]

{ #category : #accessing }
SequenceableCollection >> size [
	^ self subclassResponsibility
]

{ #category : #sorting }
SequenceableCollection >> sort [
	"Sort this collection into ascending order using the '<=' operator."

	self sort: [:a :b | a <= b]
]

{ #category : #sorting }
SequenceableCollection >> sort: aSortBlock [ 
	"Sort this array using aSortBlock. The block should take two arguments
	and return true if the first element should preceed the second one."

	self
		mergeSortFrom: 1
		to: self size
		by: aSortBlock
]

{ #category : #sorting }
SequenceableCollection >> sorted [
	"Return a new sequenceable collection which contains the same elements as self but its 
elements are sorted in ascending order using the #'<=' operator."
	
	^self sorted: [ :a :b| a <= b ]
]

{ #category : #sorting }
SequenceableCollection >> sorted: aSortBlockOrNil [
	"Return a new sequenceable collection which contains the same elements as self but its 
elements are sorted by aSortBlockOrNil. The block should take two arguments and return true if 
the first element should preceed the second one. If aSortBlock is nil then <= is used for 
comparison."
	
	^self copy sort: aSortBlockOrNil
]

{ #category : #splitjoin }
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

	aBlock value: oldPosition value: aSequenceableCollection size.


]

{ #category : #splitjoin }
SequenceableCollection >> splitOn: splitter [ 
	"Split a collection of objects based on a splitter. splitter - can be a subsequence, a Block or a Regex (String receiver only). Any other object used as a splitter is treated as an Array containing that object."
	"(#(1 2 3 3 4 1 2 3 5 4 6) splitOn: 4 >>> #(#(1 2 3 3) #(1 2 3 5) #(6))asOrderedCollection"
	"(#(1 2 3 3 4 1 2 3 3 5 6) splitOn: #(3 3)) >>> #(#(1 2) #(4 1 2) #(5 6)) asOrderedCollection"
	"(#(2 2 3 3 4 1 2 3 3 5) splitOn: [:each | each > 3])>>> #(#(2 2 3 3) #(1 2 3 3) #()) asOrderedCollection"
	
	^ splitter split: self
]

{ #category : #splitjoin }
SequenceableCollection >> splitOn: splitter do: aBlock [
	"Perform an action specified as aBlock to each of the elements of the receiver that have been split using the splitter argument. splitter - can be a subsequence, a Block or a Regex (String receiver only). Any other object used as a splitter is treated as an Array containing that object."
	"(String streamContents: [:s | 'Pharo is cool' splitOn: Character space do: [:each | s << each ]])>>>'Pharoiscool'"
	
	^ splitter split: self do: aBlock
]

{ #category : #splitjoin }
SequenceableCollection >> splitOn: splitter indicesDo: aBlock [
	"Perform an action specified as aBlock (with a start and end argument) to each of the indices of the receiver element that have been identified by splitting the receiver using the splitter argument. splitter - can be a subsequence, a Block or a Regex (String receiver only). Any other object used as a splitter is treated as an Array containing that object."
	"(String streamContents: [:s | 'Pharo is cool' splitOn: Character space indicesDo: [ :start :end | s << 's:' << start asString << ' ' << 'e:' << end asString << ' ' ]]) >>> 's:1 e:5 s:7 e:8 s:10 e:13 '"
	
	
	^ splitter split: self indicesDo: aBlock
]

{ #category : #accessing }
SequenceableCollection >> swap: oneIndex with: anotherIndex [ 
	"Move the element at oneIndex to anotherIndex, and vice-versa."

	| element |
	element := self at: oneIndex.
	self at: oneIndex put: (self at: anotherIndex).
	self at: anotherIndex put: element
]

{ #category : #accessing }
SequenceableCollection >> third [
	"Answer the third element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 3
]

{ #category : #enumerating }
SequenceableCollection >> with: otherCollection collect: twoArgBlock [ 
	"Collect and return the result of evaluating twoArgBlock with corresponding elements from this collection and otherCollection."
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

{ #category : #enumerating }
SequenceableCollection >> with: otherCollection do: twoArgBlock [ 
	"Evaluate twoArgBlock with corresponding elements from this collection and otherCollection."
	otherCollection size = self size ifFalse: [self errorSizeMismatch].
	1 to: self size do:
		[:index |
		twoArgBlock value: (self at: index)
				value: (otherCollection at: index)]
]

{ #category : #enumerating }
SequenceableCollection >> withIndexCollect: elementAndIndexBlock [ 
	"Just like with:collect: except that the iteration index supplies the second argument to the block."
	| result |
	result := self species new: self size.
	1 to: self size do:
		[:index | result at: index put:
		(elementAndIndexBlock
			value: (self at: index)
			value: index)].
	^ result
]

{ #category : #enumerating }
SequenceableCollection >> withIndexDo: elementAndIndexBlock [ 
	"Just like with:do: except that the iteration index supplies the second argument to the block. "
	"#(11 22 13) withIndexDo: [ :each :i | Transcript show: (each * each + i) ;cr ].
		122
		486
		172"
		
	1 to: self size do:
		[:index |
		elementAndIndexBlock
			value: (self at: index)
			value: index]
]

{ #category : #converting }
SequenceableCollection >> writeStream [
	^ WriteStream on: self
]

{ #category : #streaming }
SequenceableCollection >> writeStreamDo: aBlock [
	"Evaluates the argument with the write stream of the collection. Answers the result."
	
	"#() writeStreamDo: [ :stream | stream nextPut: '4'; space; nextPutAll: '34'. stream contents ] >>> {'4'. Character space. $3. $4}"

	^ aBlock value: self writeStream
]
