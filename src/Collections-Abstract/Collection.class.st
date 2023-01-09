"
I am the abstract superclass of all classes that represent a group of elements.

To get more information you read the explanation on Pharo by Example book in the next link: [Pharo by Example - Collections](https://github.com/SquareBracketAssociates/UpdatedPharoByExample/blob/pharo5/Collections/Collections.pillar).

Or if you want to read in PDF format, download it from [Pharo by Example](https://github.com/SquareBracketAssociates/PharoByExample80/releases/tag/continuous) and look for Chapter 13: Collections. 
"
Class {
	#name : #Collection,
	#superclass : #Object,
	#category : #'Collections-Abstract-Base'
}

{ #category : #'instance creation' }
Collection class >> empty [
	^ self new
]

{ #category : #testing }
Collection class >> isAbstract [

	^self name = #Collection
]

{ #category : #'instance creation' }
Collection class >> newFromArray: anArray [

	"Fast initialization with the items of a given array.
	This initializes elements faster that the generic withAll: or newFrom: methods.

	The main selling point is that dynamic arrays, like {1. 2. 3}, are really fast in Pharo.
	So other collections can be easily and efficiently initialized with `{1. 2. 3} asFoo` syntax.

	Important: Subclasses of Collection that redefine withAll: or newFrom: should also redefine this method
	either by having a proper implementation (specific to Arrays) or by calling the redefined versions of withAll:/newFrom:."

	"{ 1. 2. 3 } asSet >>> (Set new add: 1; add:2; add:3; yourself)"
	"{ 1. 2. 3 } asOrderedCollection >>> (OrderedCollection new add: 1; add:2; add:3; yourself)"
	"{ 1->2. 3->4 } asDictionary >>> (Dictionary new at: 1 put: 2; at: 3 put:4; yourself)"

	"({ 1. 2. 3 } as: Set) >>> (Set new add: 1; add:2; add:3; yourself)"
	"({ 1. 2. 3 } as: OrderedCollection) >>> (OrderedCollection new add: 1; add:2; add:3; yourself)"
	"({ 1->2. 3->4 } as: Dictionary) >>> (Dictionary new at: 1 put: 2; at: 3 put:4; yourself)"

	| newCollection size |
	size := anArray size.
	newCollection := self new: size.
	" This should be fast:
	1. Integer>>to:do: is inlined in the bytecode (no block).
	2. The Array>>at: callsite should be monomorph to a primitive method.
	"

	1 to: size do: [ :i | newCollection add: (anArray at: i) ].
	^ newCollection
]

{ #category : #'instance creation' }
Collection class >> with: anObject [
	"Answer an instance of me containing anObject."

	^ self empty
		add: anObject;
		yourself
]

{ #category : #'instance creation' }
Collection class >> with: firstObject with: secondObject [
	"Answer an instance of me containing the two arguments as elements."

	^ self new
		add: firstObject;
		add: secondObject;
		yourself
]

{ #category : #'instance creation' }
Collection class >> with: firstObject with: secondObject with: thirdObject [
	"Answer an instance of me containing the three arguments as elements."

	^ self new
		add: firstObject;
		add: secondObject;
		add: thirdObject;
		yourself
]

{ #category : #'instance creation' }
Collection class >> with: firstObject with: secondObject with: thirdObject with: fourthObject [
	"Answer an instance of me, containing the four arguments as the elements."

	^ self new
		add: firstObject;
		add: secondObject;
		add: thirdObject;
		add: fourthObject;
		yourself
]

{ #category : #'instance creation' }
Collection class >> with: firstObject with: secondObject with: thirdObject with: fourthObject with: fifthObject [
	"Answer an instance of me, containing the five arguments as the elements."

	^ self new
		add: firstObject;
		add: secondObject;
		add: thirdObject;
		add: fourthObject;
		add: fifthObject;
		yourself
]

{ #category : #'instance creation' }
Collection class >> with: firstObject with: secondObject with: thirdObject with: fourthObject with: fifthObject with: sixthObject [
	"Answer an instance of me, containing the six arguments as the elements."

	^ self new
		add: firstObject;
		add: secondObject;
		add: thirdObject;
		add: fourthObject;
		add: fifthObject;
		add: sixthObject;
		yourself
]

{ #category : #'instance creation' }
Collection class >> withAll: aCollection [
	"Create a new collection containing all the elements from aCollection."

	^ (self new: aCollection size)
		addAll: aCollection;
		yourself
]

{ #category : #enumerating }
Collection >> & aCollection [

	"Alias for intersection:"

	"#(1 2 3 4) & #(1 2 4 6 7) >>> #(1 2 4)"

	^ self intersection: aCollection
]

{ #category : #copying }
Collection >> , aCollection [
	"Concatenate the receiver with the argument."
	"#(1 2 3) , #(4 5 6) >>> #(1 2 3 4 5 6)"

	^self copy addAll: aCollection; yourself
]

{ #category : #enumerating }
Collection >> \ aCollection [

	"Return all the elements in self that are not in aCollection.
	Alias of difference:"

	"#(10 20 30) \ (0 to: 15) >>> #(20 30)"
	"'abc' \ 'cbe' >>> 'a'."

	^ self difference: aCollection
]

{ #category : #adapting }
Collection >> adaptToCollection: rcvr andSend: selector [
	"If I am involved in arithmetic with another Collection, return a Collection of
	the results of each element combined with the scalar in that expression.

	Is used to implement Collection>>+, Collection>>-, etc."

	"(#(10 20 30) adaptToCollection: #(0 1 2) andSend: #@) >>> {(0@10). (1@20). (2@30)}"

	(rcvr isSequenceable and: [ self isSequenceable ]) ifFalse:
		[self error: 'Only sequenceable collections may be combined arithmetically'].
	^ rcvr with: self collect:
		[:rcvrElement :myElement | rcvrElement perform: selector with: myElement]
]

{ #category : #adapting }
Collection >> adaptToNumber: rcvr andSend: selector [
	"If I am involved in arithmetic with a scalar, return a Collection of
	the results of each element combined with the scalar in that expression."

	^ self collect: [:element | rcvr perform: selector with: element]
]

{ #category : #adapting }
Collection >> adaptToPoint: rcvr andSend: selector [
	"If I am involved in arithmetic with a scalar, return a Collection of
	the results of each element combined with the scalar in that expression."

	^ self collect: [:element | rcvr perform: selector with: element]
]

{ #category : #adding }
Collection >> add: newObject [

	"Include newObject as one of the receiver's elements. Answer newObject.
	ArrayedCollections cannot respond to this message."

	"({10. 20} asOrderedCollection add: 30) >>> 30"
	"({10. 20} asOrderedCollection add: 30; yourself) >>> {10. 20. 30} asOrderedCollection"
	"({10. 20} asOrderedCollection add: 20; yourself) >>> {10. 20. 20} asOrderedCollection"

	"({10. 20} asSet add: 30; yourself) >>> {10. 20. 30} asSet"
	"({10. 20} asSet add: 20; yourself) >>> {10. 20} asSet"

	self subclassResponsibility
]

{ #category : #adding }
Collection >> add: newObject withOccurrences: anInteger [

	"Add newObject anInteger times to the receiver. Answer newObject."

	"({10. 20} asOrderedCollection add: 30 withOccurrences: 2) >>> 30"

	"({10. 20} asOrderedCollection add: 30 withOccurrences: 2; yourself) >>> {10. 20. 30. 30} asOrderedCollection"

	anInteger timesRepeat: [ self add: newObject ].
	^ newObject
]

{ #category : #adding }
Collection >> addAll: aCollection [

	"Include all the elements of aCollection as the receiver's elements. Answer
	aCollection. Actually, any object responding to #do: can be used as argument."

	"({10. 20} asOrderedCollection addAll: {20. 30. 30. 40.}) >>> {20. 30. 30. 40.}"

	"({10. 20} asOrderedCollection addAll: {20. 30. 30. 40.}; yourself) >>> {10. 20. 20. 30. 30. 40.} asOrderedCollection"

	"({10. 20} asSet addAll: {20. 30. 30. 40.}; yourself) >>> {10. 20. 30. 40.} asSet"

	aCollection do: [ :each | self add: each ].
	^ aCollection
]

{ #category : #adding }
Collection >> addAssignToFloatArray: aFloatArray [
	1 to: aFloatArray size do: [ :i | aFloatArray at: i put: (aFloatArray at: i) + (self at: i) ].
	^ aFloatArray
]

{ #category : #adding }
Collection >> addIfNotPresent: anObject [

	"Include anObject as one of the receiver's elements, but only if there
	is no such element already. Anwser anObject."

	"({10. 20} asOrderedCollection addIfNotPresent: 30) >>> 30"

	"({10. 20} asOrderedCollection addIfNotPresent: 30; yourself) >>> ({10. 20. 30} asOrderedCollection)"

	"({10. 20} asOrderedCollection addIfNotPresent: 20; yourself) >>> ({10. 20.} asOrderedCollection)"

	(self includes: anObject) ifFalse: [ self add: anObject ].
	^ anObject
]

{ #category : #enumerating }
Collection >> allSatisfy: aBlock [

	"Evaluate aBlock with the elements of the receiver.
	If aBlock returns false for any element return false.
	Otherwise return true."

	"(#(1 2) allSatisfy: [ :each | each even ]) >>> false"
	"(#(2 4) allSatisfy: [ :each | each even ]) >>> true"

	"('hello' allSatisfy: #isLetter) >>> true"
	"('hello!' allSatisfy: #isLetter) >>> false"

	"((3 to: 8 by: 2) allSatisfy: #isPrime) >>> true"
	"((3 to: 9 by: 2) allSatisfy: #isPrime) >>> false"

	"(#() allSatisfy: [false]) >>> true"

	self do: [ :each | (aBlock value: each) ifFalse: [ ^ false ] ].
	^ true
]

{ #category : #accessing }
Collection >> anyOne [

	"Answer a representative sample of the receiver. It raises an error when the collection is empty. This method can be helpful when needing to preinfer the nature of the contents of semi-homogeneous collections."

	"#(10 20 30) anyOne >>> 10"
	"'hello' anyOne >>> $h"
	"(50 to: 10 by: -5) anyOne >>> 50"

	"
	([#() anyOne] on: SubscriptOutOfBounds do: [ :ex | 'whatever' ]) >>> 'whatever'
	"

	self emptyCheck.
	self do: [ :each | ^ each ]
]

{ #category : #enumerating }
Collection >> anySatisfy: aBlock [

	"Evaluate aBlock with the elements of the receiver.
	If aBlock returns true for any element return true.
	Otherwise return false."

	"(#(1 3) anySatisfy: [ :each | each even ]) >>> false"
	"(#(1 2) anySatisfy: [ :each | each even ]) >>> true"

	"('hello world!' anySatisfy: [ :each | each isLetter ]) >>> true"
	"('hello world!' anySatisfy: [ :each | each isDigit ]) >>> false"

	"((4 to: 9) anySatisfy: #isPrime) >>> true"
	"((4 to: 50 by: 2) anySatisfy: #isPrime) >>> false"

	"(#() anySatisfy: [ true ]) >>> false"

	self do: [ :each | (aBlock value: each) ifTrue: [ ^ true ] ].
	^ false
]

{ #category : #converting }
Collection >> asArray [

	"Answer an Array whose elements are the elements of the receiver.
	Implementation note: Cannot use ''Array withAll: self'' as that only
	works for SequenceableCollections which support the replacement
	primitive."

	"'ab' asArray >>> {$a. $b}"
	"(1 to: 5 by: 3) asArray >>> {1. 4}"

	"#(1 2) asArray == #(1 2) >>> true"

	"'' asArray >>> #()"
	"(10 to: 5) asArray >>> #()"

	| newArray index |
	newArray := Array new: self size.
	index := 0.
	self do: [ :each | newArray at: (index := index + 1) put: each ].
	^ newArray
]

{ #category : #converting }
Collection >> asBag [

	"Answer a Bag whose elements are the elements of the receiver."

	"{1. 2} asBag =    {2. 1} asBag >>> true"
	"{1. 2} asBag = {1. 2. 2} asBag >>> false"
	"{1. 2} asBag = {1. 2. 3} asBag >>> false"
	"#() asBag = Bag new >>> true"

	^ Bag withAll: self
]

{ #category : #converting }
Collection >> asByteArray [
	"Answer a ByteArray whose elements are the elements of the receiver.
	Implementation note: Cannot use ''ByteArray withAll: self'' as that only
	works for SequenceableCollections which support the replacement
	primitive."

	"{1. 2} asByteArray >>> #[1 2]"
	"#[1 2] asByteArray == #[1 2] >>> true"
	"'foo' asByteArray >>> #[102 111 111]"
	"#() asByteArray >>> #[]"

	| array index |
	array := ByteArray new: self size.
	index := 0.
	self do: [:each | array at: (index := index + 1) put: each].
	^ array
]

{ #category : #converting }
Collection >> asCharacterSet [
	"Answer a CharacterSet whose elements are the unique elements of the receiver.
	The reciever should only contain characters."

	^ CharacterSet newFrom: self
]

{ #category : #printing }
Collection >> asCommaString [

	"Return collection printed as 'a, b, c' "

	"#('a' 'b' 'c') asCommaString >>> 'a, b, c'"
	"#('a') asCommaString >>> 'a'"
	"#() asCommaString >>> ''"
	"'foo' asCommaString >>> 'f, o, o'"
	"(10 to: 25 by: 5) asCommaString >>> '10, 15, 20, 25'"

	^ String streamContents: [ :s | self asStringOn: s delimiter: ', ' ]
]

{ #category : #printing }
Collection >> asCommaStringAnd [

	"Return collection printed as 'a, b and c' "

	"#( 'a' 'b' 'c') asCommaStringAnd >>> 'a, b and c'"
	"#('a') asCommaStringAnd >>> 'a'"
	"#() asCommaStringAnd >>> ''"
	"'foo' asCommaStringAnd >>> 'f, o and o'"
	"(10 to: 25 by: 5) asCommaStringAnd >>> '10, 15, 20 and 25'"

	^String streamContents: [:s | self asStringOn: s delimiter: ', ' last: ' and ']
]

{ #category : #converting }
Collection >> asDictionary [

	"Answers a Dictionary based on collection of Associations."

	"{'one' -> 1. 'two' ->2} asDictionary >>> (Dictionary with: 'one' -> 1 with: 'two' ->2)"
	"{'two' ->2. 'one' -> 1} asDictionary >>> (Dictionary with: 'one' -> 1 with: 'two' ->2)"
	"#() asDictionary >>> Dictionary new"

	^ self as: Dictionary
]

{ #category : #converting }
Collection >> asIdentitySet [

	"Return a new IdentitySet based on self."

	"{ 2. 1+1 } asIdentitySet size >>> 1"
	"{ 'foo'. ('fo','o') } asIdentitySet size >>> 2"

	^(IdentitySet new: self size) addAll: self; yourself
]

{ #category : #converting }
Collection >> asMultilineString [

	^ String streamContents: [ :stream |
		  self
			  do: [ :each | stream nextPutAll: each asString ]
			  separatedBy: [ stream nextPut: Character cr ] ]
]

{ #category : #converting }
Collection >> asNewArray [

	"Like asArray: but return a copy if self is already an Array.
	This ensures that the result is always a new Array"

	"'foo' asNewArray >>> 'foo' asArray"
	"|a| a := #(1 2 3). a asNewArray == a >>> false"

	^ self asArray
]

{ #category : #converting }
Collection >> asOrderedCollection [

	"Answer an OrderedCollection whose elements are the elements of the
	receiver. The order in which elements are added depends on the order
	in which the receiver enumerates its elements. In the case of unordered
	collections, the ordering is not necessarily the same for multiple
	requests for the conversion."

	"(10 to: 25 by: 5) asOrderedCollection >>> #(10 15 20 25) asOrderedCollection"

	"'foo' asOrderedCollection = #($f $o $o) asOrderedCollection >>> true"
	"'foo' asOrderedCollection = #($o $o $f) asOrderedCollection >>> false"

	^ self as: OrderedCollection
]

{ #category : #converting }
Collection >> asOrderedDictionary [

	"Answers a Dictionary based on collection of Associations."

	"{'one' -> 1. 'two' ->2} asOrderedDictionary keys first >>> 'one'"

	^ self as: OrderedDictionary
]

{ #category : #converting }
Collection >> asSet [
	"Answer a Set whose elements are the unique elements of the receiver."

	"{1. 2} asSet =    {2. 1} asSet >>> true"
	"{1. 2} asSet = {1. 2. 2} asSet >>> true"
	"{1. 2} asSet = {1. 2. 3} asSet >>> false"
	"{} asSet = Set new >>> true"

	^ Set withAll: self
]

{ #category : #converting }
Collection >> asSortedCollection [

	"Answer a SortedCollection whose elements are the elements of the receiver. The sort order is the default less than or equal. Note that you should use #sorted if you don't really need a SortedCollection, but a sorted collection."

	"'bar' asSortedCollection asArray >>> {$a. $b. $r}."

	"('bar' asSortedCollection add: $c; yourself) asArray >>> {$a. $b. $c. $r}."

	^ self as: SortedCollection
]

{ #category : #converting }
Collection >> asSortedCollection: aSortBlock [

	"Answer a SortedCollection whose elements are the elements of the receiver. The sort order is defined by the argument, aSortBlock. Note that it is better to use #sorted if you don't really need a SortedCollection, but a sorted collection!!"

	"('bar' asSortedCollection: [:x :y| x>y ]) asArray >>> {$r. $b. $a}."

	| aSortedCollection |
	aSortedCollection := SortedCollection new: self size.
	aSortedCollection sortBlock: aSortBlock.
	aSortedCollection addAll: self.
	^ aSortedCollection
]

{ #category : #printing }
Collection >> asStringOn: aStream delimiter: delimString [

	"Print elements on a stream separated
	with a delimiter String like: 'a, b, c'
	Uses #asString instead of #print:."

	"(String streamContents: [:s|
		'abcd' asStringOn: s delimiter: '->'])
	>>> 'a->b->c->d'"

	self
		do: [ :elem | aStream nextPutAll: elem asString ]
		separatedBy: [ aStream nextPutAll: delimString ]
]

{ #category : #printing }
Collection >> asStringOn: aStream delimiter: delimString last: lastDelimString [

	"Print elements on a stream separated
	with a delimiter between all the elements and with
	a special one before the last like: 'a, b and c'.
	Uses #asString instead of #print:

	Note: Feel free to improve the code to detect the last element."

	"(String streamContents: [:s|
		'abcd' asStringOn: s delimiter: ', ' last: ' and '])
	>>> 'a, b, c and d'"

	| n sz |
	n := 1.
	sz := self size.
	self do: [:elem |
		n := n + 1.
		aStream nextPutAll: elem asString]
	separatedBy: [
		aStream nextPutAll: (n = sz ifTrue: [lastDelimString] ifFalse: [delimString])]
]

{ #category : #enumerating }
Collection >> associationsDo: aBlock [

	"Evaluate aBlock for each of the receiver's elements (key/value
	associations).  If any non-association is within, the error is not caught now,
	but later, when a key or value message is sent to it.

	The point of this method it to do the *right thing* on Dictionaries and related classes.
	"

	"(String streamContents: [:s|
		{'one'->1. 'two'->2} associationsDo: [:a|
			s << a key << ':' << a value asString << ';']])
	>>> 'one:1;two:2;'"

	"(String streamContents: [:s|
		{'one'->1. 'two'->2} asOrderedDictionary associationsDo: [:a|
			s << a key << ':' << a value asString << ';']])
	>>> 'one:1;two:2;'"

	self do: aBlock
]

{ #category : #accessing }
Collection >> capacity [
	"Answer the current capacity of the receiver."

	^ self size
]

{ #category : #enumerating }
Collection >> collect: aBlock [
	"Evaluate aBlock with each of the receiver's elements as the argument.
	Collect the resulting values into a collection like the receiver. Answer
	the new collection.

	Note: the returned collection is of the same species. If this species
	does not accomdate the values of aBlock, look at collect:as:"

	"(#(10 20 30) collect: [:e | e+1]) >>> #(11 21 31)"
	"({10@20. 30@0} collect: [:e | e x]) >>> #(10 30)"
	"('Hello, world!' collect: [:e | e isLetter ifTrue: e ifFalse: $-]) >>> 'Hello--world-'"
	"((1 to: 10) collect: [:i| i gcd: 6]) >>> #(1 2 3 2 1 6 1 2 3 2)"
	"(#() collect: [:x | x+1]) >>> #()"

	| newCollection |
	newCollection := self species new.
	self do: [:each | newCollection add: (aBlock value: each)].
	^ newCollection
]

{ #category : #enumerating }
Collection >> collect: aBlock as: aClass [
	"Evaluate aBlock with each of the receiver's elements as the argument.
	Collect the resulting values into an instance of aClass. Answer the resulting collection."

	"(#(10 15 22 789) collect: [:x | x//10] as: Set) >>> #(1 2 78) asSet"
	"('hello' collect: #asciiValue as: Array) >>> #(104 101 108 108 111)"

	^(aClass new: self size) fillFrom: self with: aBlock
]

{ #category : #enumerating }
Collection >> collect: aBlock into: aCollection [

	"Evaluate aBlock with each of the receiver's elements as the argument.
	Collect the resulting values into aCollection. Answer aCollection."

	"({10. 20} collect: [:x|x+1] into: {'a'. 'b'. 'c'}) >>> {11. 21. 'c'.}"
	"('hello' collect: #asciiValue into: Set new) >>> #(104 101 108 111) asSet"

	^aCollection fillFrom: self with: aBlock
]

{ #category : #enumerating }
Collection >> collect: collectBlock thenDo: doBlock [
	"Utility method to improve readability."

	^ self do: [ :each|
		doBlock value: (collectBlock value: each)]
]

{ #category : #enumerating }
Collection >> collect: collectBlock thenReject: selectBlock [
	"Utility method to improve readability."

	^ (self collect: collectBlock) reject: selectBlock
]

{ #category : #enumerating }
Collection >> collect: collectBlock thenSelect: selectBlock [
	"Utility method to improve readability."

	^ (self collect: collectBlock) select: selectBlock
]

{ #category : #printing }
Collection >> collectionSizeThreshold [
	^ 3000
]

{ #category : #testing }
Collection >> contains: aBlock [

	"For compatibility, please use #anySatisfy: instead!
	And maybe you are looking for includes:"

	^ self anySatisfy: aBlock
]

{ #category : #'filter streaming' }
Collection >> contents [
	^ self
]

{ #category : #copying }
Collection >> copyEmpty [

	"Return a new empty collection based on the species"

	"{1@2} copyEmpty >>> #()"
	"'hello' copyEmpty >>> ''"
	"(1 to: 10) copyEmpty >>> #()"

	^ self species new
]

{ #category : #copying }
Collection >> copyWith: newElement [
	"Answer a new collection with newElement added (as last
	element if sequenceable)."

	"(#(10 20) asSet copyWith: 30) >>> #(10 20 30) asSet"
	"(#(10 20) asSet copyWith: 20) >>> #(10 20) asSet"

	^ self copy
		add: newElement;
		yourself
]

{ #category : #copying }
Collection >> copyWithAll: aCollection [

	^ self , aCollection
]

{ #category : #copying }
Collection >> copyWithDependent: newElement [
	"Answer a new collection with newElement added (as last
	element if sequenceable)."
	^self copyWith: newElement
]

{ #category : #copying }
Collection >> copyWithout: oldElement [
	"Answer a copy of the receiver that does not contain any
	elements equal to oldElement."
	"('fred the bear' copyWithout: $e) >>> 'frd th bar'"
	"(#(2 3 4 5 5 6) copyWithout: 5) >>> #(2 3 4 6)"
	"((10 to: 13) copyWithout: 11) >>> #(10 12 13)"


	^ self reject: [:each | each = oldElement]
]

{ #category : #copying }
Collection >> copyWithoutAll: aCollection [
	"Answer a copy of the receiver that does not contain any elements
	equal to those in aCollection."

	"('fred the bear' copyWithoutAll: 'aeiou ') >>> 'frdthbr'"
	"(#(2 3 4 5 5 6 2) copyWithoutAll: (4 to: 10)) >>> #(2 3 2)"

	^ self reject: [:each | aCollection includes: each]
]

{ #category : #copying }
Collection >> copyWithoutDuplicates [
	"Answer a copy of the receiver without any duplicated elements"
	"(#(2 3 4 4 5 6) copyWithoutDuplicates asSet)  >>> #(2 3 4 5 6) asSet"
	"(#('do' 'la' 'si' 'do' 'la') copyWithoutDuplicates) >>> #('la' 'do' 'si')"
	"(#(#do #la #si #do #la) copyWithoutDuplicates) >>> #(#la #do #si)"
	^ self asSet asArray
]

{ #category : #copying }
Collection >> copyWithoutFirst: anObject [
	| done |
	done := false.
	^ self reject: [ :each |
		(each = anObject
			and: [ done not ])
				and: [ done := true ]]
]

{ #category : #copying }
Collection >> copyWithoutFirstOccurrenceOf: anObject [

	| done |
	done := false.
	^ self reject: [ :each |
		each = anObject
			and: [ done not
			and: [ done := true ] ] ]
]

{ #category : #enumerating }
Collection >> count: aBlock [

	"Evaluate aBlock with each of the receiver's elements as the argument.
	Answer the number of elements that answered true."

	"(#(1 2 3 4) count: [ :each | each even ]) >>> 2"
	"('Hello, World!' count: #isLowercase) >>> 8"

	| sum |
	sum := 0.
	self do: [ :each | (aBlock value: each) ifTrue: [ sum := sum + 1 ] ].
	^ sum
]

{ #category : #enumerating }
Collection >> detect: aBlock [

	"Evaluate aBlock with each of the receiver's elements as the argument.
	Answer the first element for which aBlock evaluates to true."

	"({1@3. 2@1. 3@6. 4@8} detect: [ :each | each x even ]) >>> (2@1)"
	"((104 to: 120) detect: #isPrime) >>> 107"
	"('Hello!' detect: #isLowercase) >>> $e"

	^ self detect: aBlock ifNone: [ self errorNotFound: aBlock ]
]

{ #category : #enumerating }
Collection >> detect: aBlock ifFound: foundBlock [
	"Evaluate aBlock with each of the receiver's elements as the argument.
	If some element evaluates aBlock to true, then cull this element into
	foundBlock.
	If no element matches the criteria then do nothing.
	Always returns self to avoid misuse and a potential isNil check on the sender."

	"|s| (#(1 2) detect: [ :each | each even ] ifFound: [ :e | s:=e*10 ]). s >>> 20"
	"|s| (#(1 3) detect: [ :each | each even ] ifFound: [ :e | s:=e*10 ]). s >>> nil"

	self
		detect: aBlock
		ifFound: foundBlock
		ifNone: [
			"Do nothing on purpose"
			 ]
]

{ #category : #enumerating }
Collection >> detect: aBlock ifFound: foundBlock ifNone: exceptionBlock [
	"Evaluate aBlock with each of the receiver's elements as the argument.
	If some element evaluates aBlock to true, then cull this element into
	foundBlock and answer the result of this evaluation.
	If none evaluate to true, then evaluate exceptionBlock."

	"(#(1 2) detect: #even ifFound: #negated ifNone: [0]) >>> -2"
	"(#(1 3) detect: #even ifFound: #negated ifNone: [0]) >>> 0"
	"('Hello' detect: #isLowercase ifFound: #uppercase ifNone: [$X]) >>> $E"
	"('LOL' detect: #isLowercase ifFound: #uppercase ifNone: [$X]) >>> $X"

	self
		do: [ :each |
			(aBlock value: each)
				ifTrue: [ ^ foundBlock cull: each ] ].
	^ exceptionBlock value
]

{ #category : #enumerating }
Collection >> detect: aBlock ifNone: exceptionBlock [
	"Evaluate aBlock with each of the receiver's elements as the argument.
	Answer the first element for which aBlock evaluates to true. If none
	evaluate to true, then evaluate the argument, exceptionBlock."

	"(#(1 2) detect: #even ifNone: [0]) >>> 2"
	"(#(1 3) detect: #even ifNone: [0]) >>> 0"
	"('Hello' detect: #isLowercase ifNone: [$X]) >>> $e"
	"('LOL' detect: #isLowercase ifNone: [$X]) >>> $X"

	^ self detect: aBlock ifFound: [ :element | element ] ifNone: exceptionBlock
]

{ #category : #enumerating }
Collection >> detectMax: aBlock [
	"Evaluate aBlock with each of the receiver's elements as the argument.
	Answer the element for which aBlock evaluates to the highest magnitude.
	If collection empty, return nil.  This method might also be called elect:."

	"({ 2@6 . -4@3 . 10@ -3 } detectMax: [ :p | p x ]) >>> (10@ -3)"
	"({ 2@6 . -4@3 . 10@ -3 } detectMax: [ :p | p y ]) >>> (2@6)"
	"((10 to: 20) detectMax: [ :p | p gcd: 6 ]) >>> 12"
	"('Hello' detectMax: #asciiValue) >>> $o"

	| maxElement maxValue |
	self do: [:each | | val |
		maxValue == nil
			ifFalse: [
				(val := aBlock value: each) > maxValue ifTrue: [
					maxElement := each.
					maxValue := val]]
			ifTrue: ["first element"
				maxElement := each.
				maxValue := aBlock value: each].
				"Note that there is no way to get the first element that works
				for all kinds of Collections.  Must test every one."].
	^ maxElement
]

{ #category : #enumerating }
Collection >> detectMin: aBlock [
	"Evaluate aBlock with each of the receiver's elements as the argument.
	Answer the element for which aBlock evaluates to the lowest number.
	If collection empty, return nil."

	"({ 2@6 . -4@3 . 10@ -3 } detectMin: [ :p | p x ]) >>> (-4@3)"
	"({ 2@6 . -4@3 . 10@ -3 } detectMin: [ :p | p y ]) >>> (10@ -3)"
	"('Hello!' detectMin: #asciiValue) >>> $!"

	| minElement minValue |
	self do: [:each | | val |
		minValue == nil
			ifFalse: [
				(val := aBlock value: each) < minValue ifTrue: [
					minElement := each.
					minValue := val]]
			ifTrue: ["first element"
				minElement := each.
				minValue := aBlock value: each].
				"Note that there is no way to get the first element that works
				for all kinds of Collections.  Must test every one."].
	^ minElement
]

{ #category : #enumerating }
Collection >> difference: aCollection [
	"Answer the set theoretic difference of two collections. Pay attention that the difference is not commutative, hence the order is important."

	"(#(a b c d e f) difference:  #(a b z k))>>> #(#c #d #e #f)"

	"(#(a b z k) difference: #(a b c d e f)) >>> #(#z #k)"

	"('hello' difference: 'aeiou') >>> 'hll'"

	| set |
	set := self asSet
		removeAllFoundIn: aCollection;
		yourself.
	^ self species withAll: set asArray
]

{ #category : #printing }
Collection >> displayStringOn: stream [
	self printNameOn: stream.
	stream
		space;
		nextPut: $[;
		print: self size;
		nextPutAll: (' item' asPluralBasedOn: self size);
		nextPut: $];
		space.
	self size <= self collectionSizeThreshold
		ifTrue: [ self printElementsOn: stream ]
]

{ #category : #enumerating }
Collection >> do: aBlock [
	"Evaluate aBlock with each of the receiver's elements as the argument.

	This is the general foreach method, but for most standard needs there is often a more specific and simpler method."

	"|s|
	s:=0.
	#(10 20 30) do: [:each | s := s + each].
	s >>> 60"

	"but use sum or inject:into: instead"

	"(#(10 20 30) inject: 0 into: [:s :each| s + each ]) >>> 60"
	"#(10 20 30) sum >>> 60"

	"
	(String streamContents: [:s |
		#('hello' 'the' 'world') do: [:each | s << each]])
	>>> 'hellotheworld'"

	self subclassResponsibility
]

{ #category : #enumerating }
Collection >> do: elementBlock separatedBy: separatorBlock [

	"Evaluate the elementBlock for all elements in the receiver,
	and evaluate the separatorBlock between."

	"(String streamContents: [:s | #(1 2 3) do: [:each | s << each asString] separatedBy: [s << ', ']]) >>> '1, 2, 3'"

	| beforeFirst |
	beforeFirst := true.
	self do:
		[:each |
		beforeFirst
			ifTrue: [beforeFirst := false]
			ifFalse: [separatorBlock value].
		elementBlock value: each]
]

{ #category : #enumerating }
Collection >> do: aBlock without: anItem [

	"Enumerate all elements in the receiver.
	Execute aBlock for those elements that are not equal to the given item"

	"(String streamContents: [:s | #(10 20 30) do: [:each | s << each asString] without: 20]) >>> '1030'"

	^ self do: [ :each | anItem = each ifFalse: [ aBlock value: each ] ]
]

{ #category : #enumerating }
Collection >> doWithIndex: elementAndIndexBlock [

	"Use the new version with consistent naming"

	^ self withIndexDo: elementAndIndexBlock
]

{ #category : #private }
Collection >> emptyCheck [

	"Signal CollectionIsEmpty if the collection is empty"

	"#(10 20) emptyCheck >>> #(10 20)"
	"([#() emptyCheck] on: CollectionIsEmpty do: [ 'oops' ]) >>> 'oops'"

	self isEmpty ifTrue: [self errorEmptyCollection]
]

{ #category : #private }
Collection >> errorEmptyCollection [
	"Signal a CollectionIsEmpty exception"

	CollectionIsEmpty signalWith: self
]

{ #category : #private }
Collection >> errorNotFound: anObject [
	"Raise a NotFound exception."

	NotFound signalFor: anObject
]

{ #category : #private }
Collection >> errorSizeMismatch [
	"Signal a SizeMismatch exception"

	SizeMismatch signal
]

{ #category : #private }
Collection >> fillFrom: aCollection with: aBlock [
	"Evaluate aBlock with each of aCollections's elements as the argument.
	Collect the resulting values into self. Answer self."

	"(#(10 20) asOrderedCollection fillFrom: 'ABC' with: [:x|x asciiValue]) >>> #(10 20 65 66 67) asOrderedCollection"
	"(#(10 20) asSet fillFrom: 'ABC' with: [:x|x asciiValue]) >>> #(10 20 65 66 67) asSet"
	"({64->$@} asDictionary fillFrom: 'AB' with: [:x|x asciiValue -> x]) >>> {64->$@. 65->$A. 66->$B} asDictionary"

	aCollection do: [ :each |
		self add: (aBlock value: each) ]
]

{ #category : #enumerating }
Collection >> findFirstInByteString: aByteString startingAt: start [
	"Find the index of first character starting at start in aByteString that is included in the receiver.
	Default is to use a naive algorithm.
	Subclasses might want to implement a more efficient scheme.

	Return 0 if not found."

	"('aeiou' findFirstInByteString: 'hello world' startingAt: 1) >>> 2."
	"('aeiou' findFirstInByteString: 'hello world' startingAt: 3) >>> 5."
	"('aeiou' findFirstInByteString: 'hello world' startingAt: 9) >>> 0."

	start to: aByteString size do:
		[:index |
		(self includes: (aByteString at: index)) ifTrue: [^ index]].
	^ 0
]

{ #category : #enumerating }
Collection >> flatCollect: aBlock [
	"Evaluate aBlock for each of the receiver's elements and answer the
	list of all resulting values flatten one level. Assumes that aBlock returns some kind
	of collection for each element. Equivalent to the lisp's mapcan"

	"( #((3 4) (1 2)) flatCollect: [:each | each ] ) >>> #(3 4 1 2)"
	"( #(3 4 1 2) flatCollect: [:each | { each. each*10 } ] ) >>> #(3 30 4 40 1 10 2 20)"

	^ self flatCollect: aBlock as: self species
]

{ #category : #enumerating }
Collection >> flatCollect: aBlock as: aCollectionClass [
	"Evaluate aBlock for each of the receiver's elements and answer the
	list of all resulting values flatten one level. Assumes that aBlock returns some kind
	of collection for each element. Equivalent to the lisp's mapcan"

	"(#(1 2 3) flatCollect: [:each | { each. each+1 } ] as: Set) >>> #(1 2 3 4) asSet"
	"(#(65 66 67) flatCollect: [:each | { each asCharacter. each asCharacter asLowercase } ] as: String) >>> 'AaBbCc'"

	| col |
	col := OrderedCollection new: self size.
	self do: [ :each | col addAll: (aBlock value: each) ].
	^ aCollectionClass withAll: col
]

{ #category : #enumerating }
Collection >> flatCollectAsSet: aBlock [
	"Evaluate aBlock for each of the receiver's elements and answer the
	list of all resulting values flatten one level. Assumes that aBlock returns some kind
	of collection for each element. Equivalent to the lisp's mapcan"

	"( #(1 2 3) flatCollectAsSet: [:each | { each. each+1 } ] ) >>> #(1 2 3 4) asSet"

	^ self flatCollect: aBlock as: Set
]

{ #category : #enumerating }
Collection >> flattenOn: aStream [

	"See Collection>>flattened that uses this method"

	self do: [ :each | each flattenOn: aStream ]
]

{ #category : #enumerating }
Collection >> flattened [

	"Flattens a collection of collections (no matter how many levels of collections exist). Strings are considered atoms and, as such, won't be flattened"

	"( #(1 #(2 3) #(4 #(5))) flattened ) >>> #(1 2 3 4 5)"

	"( #('string1' #('string2' 'string3')) flattened ) >>> #('string1' 'string2' 'string3')"

	^ Array streamContents: [ :stream | self flattenOn: stream ]
]

{ #category : #enumerating }
Collection >> fold: binaryBlock [
	"Alias of Collection>>reduce"
	"Evaluate the block with the first two (or more) elements of the receiver, then with the result of the first evaluation and the next element, and so on.  Answer the result of the final evaluation. If the receiver is empty, raise an error. If the receiver has a single element, answer that element."
	"( #('if' 'it' 'is' 'to' 'be' 'it' 'is' 'up' 'to' 'me') fold: [:a :b | a, ' ', b] ) >>> 'if it is to be it is up to me'"

	^self reduce: binaryBlock
]

{ #category : #enumerating }
Collection >> gather: aBlock [
	"This method is kept for compatibility reasons, use flatCollect: instead."

	^ self flatCollect: aBlock
]

{ #category : #enumerating }
Collection >> groupedBy: aBlock [
	"Answer a dictionary whose keys are the result of evaluating aBlock for all my elements, and the value for each key is the selection of my elements that evaluated to that key. Uses species."

	"(#(1 2 3 4 5) groupedBy: [ :v | v odd ]) asString
		>>> 'an OrderedDictionary(true->#(1 3 5) false->#(2 4))'"

	| groups |
	groups := OrderedDictionary new.
	self do: [ :each |
		(groups at: (aBlock value: each) ifAbsentPut: [ OrderedCollection new ]) add: each ].
	self species ~~ OrderedCollection ifTrue: [
		groups associationsDo: [ :association |
			association value: (self species withAll: association value) ]].
	^ groups
]

{ #category : #enumerating }
Collection >> groupedBy: aBlock having: aSelectionBlock [
	"Like in SQL operation - Split the recievers contents into collections of
	elements for which keyBlock returns the same results, and return those
	collections allowed by selectBlock."

	"In the following example, the group `3->#(34)` is filtered out because there is not 2 elements."

	"(#(1 5 21 28 34)
		groupedBy: [:x| x // 10]
		having: [:v| v size = 2])
	>>> {0->#(1 5). 2->#(21 28)} asOrderedDictionary"

	^ (self groupedBy: aBlock) select: aSelectionBlock
]

{ #category : #comparing }
Collection >> hash [
	"Answer an integer hash value for the receiver such that,
	  -- the hash value of an unchanged object is constant over time, and
	  -- two equal objects have equal hash values"

	| hash |

	hash := self species hash.
	self size <= 10 ifTrue:
		[self do: [:elem | hash := hash bitXor: elem hash]].
	^hash bitXor: self size hash
]

{ #category : #testing }
Collection >> identityIncludes: anObject [
	"Answer whether anObject is one of the receiver's elements."

	"({#hello. #world} identityIncludes: #hello) >>> true"
	"({'hel','lo'. 'world'} identityIncludes: 'hello') >>> false"
	"({'hel','lo'. 'world'} includes: 'hello') >>> true"

	self do: [:each | anObject == each ifTrue: [^true]].
	^false
]

{ #category : #testing }
Collection >> ifEmpty: aBlock [
	"Evaluate the given block, answering its value if the receiver is empty, otherwise answer the receiver."

	"(#(1 2) ifEmpty: [0]) >>> #(1 2)"
	"(#() ifEmpty: [0]) >>> 0"
	"('' ifEmpty: ['(nil)']) >>> '(nil)'"

	"Note that the fact that this method returns its receiver in case the receiver is not empty allows one to write expressions like the following ones: self classifyMethodAs: (myProtocol ifEmpty: ['As yet unclassified'])"

	^ self isEmpty
		ifTrue: [ aBlock value ]
		ifFalse: [ self ]
]

{ #category : #testing }
Collection >> ifEmpty: emptyBlock ifNotEmpty: notEmptyBlock [
	"Evaluate emptyBlock if I'm empty, notEmptyBlock otherwise"
	"If the notEmptyBlock has an argument, eval with the receiver as its argument"

	^ self isEmpty
		ifTrue: [ emptyBlock value ]
		ifFalse: [ notEmptyBlock cull: self ]
]

{ #category : #testing }
Collection >> ifNotEmpty: aBlock [
	"Evaluate the given block with the receiver as argument, answering its value
	unless the receiver is empty, in which case answer the receiver."

	"(#(10 20) ifNotEmpty: [:x| x average]) >>> 15"
	"(#() ifNotEmpty: [:x| x average]) >>> #()"

	^ self isEmpty
		ifTrue: [ self ]
		ifFalse: [ aBlock cull: self ]
]

{ #category : #testing }
Collection >> ifNotEmpty: notEmptyBlock ifEmpty: emptyBlock [
	"Evaluate emptyBlock if I'm empty, notEmptyBlock otherwise
	 If the notEmptyBlock has an argument, eval with the receiver as its argument"

	^ self isEmpty
		ifTrue: [ emptyBlock value ]
		ifFalse: [ notEmptyBlock cull: self ]
]

{ #category : #testing }
Collection >> includes: anObject [
	"Answer whether anObject is one of the receiver's elements."

	"(#(10 20 30) includes: 20) >>> true"
	"(#(10 20 30) includes: 21) >>> false"

	"((1 to:9 by:2) includes: 3) >>> true"
	"((1 to:9 by:2) includes: 4) >>> false"

	"('Hello' includes: $l) >>> true"
	"('Hello' includes: $h) >>> false"

	"({'hello'. 'world'} includes: 'hello') >>> true"
	"({'hel','lo'. 'world'} includes: 'hello') >>> true"
	^ self anySatisfy: [:each | each = anObject]
]

{ #category : #testing }
Collection >> includesAll: aCollection [
	"Answer whether all the elements of aCollection are in the receiver."

	"(#(10 20 30) includesAll: #(30 20)) >>> true"
	"(#(10 20 30) includesAll: #(30 40)) >>> false"
	"(#(10 20 30) includesAll: #()) >>> true"
	"('hello' includesAll: 'ho') >>> true"
	"('hello' includesAll: 'hop') >>> false"

	aCollection do: [:elem | (self includes: elem) ifFalse: [^ false]].
	^ true
]

{ #category : #testing }
Collection >> includesAny: aCollection [
	"Answer whether any element of aCollection is one of the receiver's elements."

	"(#(10 20 30) includesAny: #(30 40)) >>> true"
	"(#(10 20 30) includesAny: #(50 40)) >>> false"
	"(#(10 20 30) includesAny: #()) >>> false"
	"('hello' includesAny: 'hop') >>> true"
	"('hello' includesAny: 'pwn') >>> false"

	aCollection do: [:elem | (self includes: elem) ifTrue: [^ true]].
	^ false
]

{ #category : #testing }
Collection >> includesAnyOf: aCollection [

	self flag: 'use includesAny: instead'.

	^ self includesAny: aCollection
]

{ #category : #testing }
Collection >> includesSubstringAnywhere: testString [
	"Answer whether the receiver includes, anywhere in its nested structure, a string that has testString as a substring"

	"(#(first (second third) ((allSentMessages ('Elvis' includes:))))
		includesSubstringAnywhere:  'lvi') >>> true"

	self do:
		[:element |
			(element isString)
				ifTrue:
					[(element includesSubstring: testString) ifTrue: [^ true]].
			(element isCollection)
				ifTrue:
					[(element includesSubstringAnywhere: testString) ifTrue: [^ true]]].
	^ false
]

{ #category : #enumerating }
Collection >> inject: thisValue into: binaryBlock [
	"Accumulate a running value associated with evaluating the argument, binaryBlock, with the current value of the argument, thisValue, and the receiver as block arguments."
	"Look at Collection>>reduce: for a related method."

	"(#(2r101 2r11 2r1000) inject: 0 into: [ :acc :each | acc bitXor: each ]) >>> 2r1110"

	"(#(10 20 30) inject: 0 into: [ :sum :each | sum + each ]) >>> 60"
	"But use sum or sum: instead!"
	"#(10 20 30) sum >>> 60"

	"(#(10 20 30) inject: OrderedCollection new into: [ :a :e | a add: (e + 1). a ])
		>>> #(11 21 31) asOrderedCollection"
	"But use collect: or collect:as: instead!"
	"(#(10 20 30) collect: [:e| e+1]) >>> #(11 21 31)"

	| nextValue |
	nextValue := thisValue.
	self do: [:each | nextValue := binaryBlock value: nextValue value: each].
	^nextValue
]

{ #category : #enumerating }
Collection >> intersection: aCollection [
	"Answer the set theoretic intersection of two collections."

	"(#(1 2 3 4) intersection: #(3 4 5)) >>> #(3 4)"

	"(#(1 2 3 4) intersection: #()) >>> #()"

	"(#() intersection: #(1 2 3 4)) >>> #()"

	"('hello' intersection: 'world') >>> 'ol'"

	^ self species withAll: (self asSet intersection: aCollection) asArray
]

{ #category : #testing }
Collection >> isCollection [
	"Return true if the receiver is some sort of Collection and responds to basic collection messages such as #size and #do:"
	^true
]

{ #category : #testing }
Collection >> isEmpty [
	"Answer whether the receiver contains any elements."

	"{} isEmpty >>> true"
	"{{}} isEmpty >>> false"
	"'' isEmpty >>> true"
	"' ' isEmpty >>> false"
	"(1 to: 10) isEmpty >>> false"
	"(10 to: 1) isEmpty >>> true"

	^self size = 0
]

{ #category : #testing }
Collection >> isEmptyOrNil [
	"Answer whether the receiver contains any elements, or is nil.  Useful in numerous situations where one wishes the same reaction to an empty collection or to nil"

	"#() isEmptyOrNil >>> true"
	"nil isEmptyOrNil >>> true"
	"([0 isEmptyOrNil] on: MessageNotUnderstood do: ['oops']) >>> 'oops'"

	^ self isEmpty
]

{ #category : #testing }
Collection >> isNotEmpty [
	"Answer whether the receiver contains any elements."

	"#() isNotEmpty >>> false"
	"#(()) isNotEmpty >>> true"
	"'' isNotEmpty >>> false"
	"' ' isNotEmpty >>> true"

	^ self isEmpty not
]

{ #category : #testing }
Collection >> isSequenceable [

	"See SequenceableCollection"

	^ false
]

{ #category : #'math functions' }
Collection >> median [
	"Return the middle element, or as close as we can get.
	The collection must not be empty."

	"{1 . 2 . 3 . 4 . 5} median >>> 3"
	"{1 . 2 . 4 . 5} median >>> 3"
	"{1 . 2 . 5 . 5} median >>> (7/2)"
	"{3} median >>> 3"
	"([{} median] on: CollectionIsEmpty do: [ 'oops' ]) >>> 'oops'"

	self emptyCheck.
	^ self asSortedCollection median
]

{ #category : #enumerating }
Collection >> noneSatisfy: aBlock [
	"Evaluate aBlock with the elements of the receiver. If aBlock returns false for all elements return true. Otherwise return false"

	"(#(2 4 6) noneSatisfy: [:x|x odd]) >>> true"
	"(#(1 2 3) noneSatisfy: [:x|x odd]) >>> false"
	"('hello!' noneSatisfy: #isUppercase) >>> true"
	"('hello!' noneSatisfy: #isLetter) >>> false"
	"(#() noneSatisfy: ['oops']) >>> true"

	self do: [:item | (aBlock value: item) ifTrue: [^ false]].
	^ true
]

{ #category : #testing }
Collection >> notEmpty [
	"Answer whether the receiver contains any elements."
	"use isNotEmpty for consistency with isEmpty"

	^ self isEmpty not
]

{ #category : #enumerating }
Collection >> occurrencesOf: anObject [
	"Answer how many of the receiver's elements are equal to anObject."

	"(#(10 20 10 30) occurrencesOf: 10) >>> 2"
	"('hello world' occurrencesOf: $l) >>> 3"
	"('hello world' occurrencesOf: 10) >>> 0"

	| tally |
	tally := 0.
	self do: [:each | anObject = each ifTrue: [tally := tally + 1]].
	^tally
]

{ #category : #printing }
Collection >> printElementsOn: aStream [
	"List elements betwen () and separated by spaces.
	Is used by printOn: and other related printing methods."

	"(String streamContents: [:s|
		{10. 'hello'} printElementsOn: s])
		>>> '(10 ''hello'')'"
	"(String streamContents: [:s|
		#() printElementsOn: s])
		>>> '()'"

	"Note: The original code used #skip:, but some streams do not support that,
	 and we don't really need it."

	aStream nextPut: $(.
	self do: [:element | aStream print: element] separatedBy: [aStream space].
	aStream nextPut: $)
]

{ #category : #printing }
Collection >> printNameOn: aStream [
	super printOn: aStream
]

{ #category : #printing }
Collection >> printOn: aStream [
	"Append a sequence of characters that identify the receiver to aStream."

	self printNameOn: aStream.
	self printElementsOn: aStream
]

{ #category : #printing }
Collection >> printOn: aStream delimiter: delimString [
	"Print elements on a stream separated
	with a delimiter String like: 'a, b, c'"

	"(String streamContents: [:s|
		{ 10. 'hello'. $x } printOn: s delimiter: ', '])
		>>> '10, ''hello'', $x'"

	self do: [:elem | aStream print: elem] separatedBy: [aStream nextPutAll: delimString]
]

{ #category : #printing }
Collection >> printOn: aStream delimiter: delimString last: lastDelimString [
	"Print elements on a stream separated
	with a delimiter between all the elements and with
	a special one before the last like: 'a, b and c'"

	"(String streamContents: [:s|
		{ 10. 'hello'. $x } printOn: s delimiter: ', ' last: ' & '])
		>>> '10, ''hello'' & $x'"

	"Note: Feel free to improve the code to detect the last element."

	| n sz |
	n := 1.
	sz := self size.
	self do: [:elem |
		n := n + 1.
		aStream print: elem]
	separatedBy: [
		n = sz
			ifTrue: [aStream nextPutAll: lastDelimString]
			ifFalse: [aStream nextPutAll: delimString]]
]

{ #category : #enumerating }
Collection >> reduce: aBlock [
	"Fold the result of the receiver into aBlock. The argument aBlock must take two or more arguments. It applies the argument, binaryBlock cumulatively to the elements of the receiver. For sequenceable collections the elements will be used in order, for unordered collections the order is unspecified."

	"( #(1 2 3) asSet reduce: [ :a :b | a + b ] ) >>> (1 + 2 + 3)"
	"( #(1 2 3 4 5) asSet reduce: [ :a :b :c | a + b + c ] ) >>> (1 + 2 + 3 + 4 + 5)"

	"Maybe look at the related method Collection>>inject:into:"

	^self asOrderedCollection reduce: aBlock
]

{ #category : #enumerating }
Collection >> reject: aBlock [

	"Evaluate aBlock with each of the receiver's elements as the argument. Collect into a new collection like the receiver only those elements for which aBlock evaluates to false. Answer the new collection."

	"(#(1 2 3 4 5) reject: #even) >>>  #(1 3 5)"
	"('Hello, World!' reject: #isLetter) >>> ', !'"
	"((1 to: 8) reject: #isPrime) >>> #(1 4 6 8)"

	^ self select: [ :element | (aBlock value: element) == false ]
]

{ #category : #enumerating }
Collection >> reject: rejectBlock thenCollect: collectBlock [
	"Utility method to improve readability."

	"(#(1 2 3 4 5) reject: #even thenCollect: [:x|x*10]) >>> #(10 30 50)"

	^ (self reject: rejectBlock) collect: collectBlock
]

{ #category : #enumerating }
Collection >> reject: rejectBlock thenDo: doBlock [
	"Utility method to improve readability.
	Do not create the intermediate collection."

	self do: [ :each |
		(rejectBlock value: each)
			ifFalse: [ doBlock value: each ] ]
]

{ #category : #removing }
Collection >> remove: oldObject [
	"Remove oldObject from the receiver's elements. Answer oldObject
	unless no element is equal to oldObject, in which case, raise an error.
	ArrayedCollections cannot respond to this message."

	"(#(10 20 10) asOrderedCollection remove: 10; yourself) >>> #(20 10) asOrderedCollection"

	^ self remove: oldObject ifAbsent: [self errorNotFound: oldObject]
]

{ #category : #removing }
Collection >> remove: oldObject ifAbsent: anExceptionBlock [
	"Remove oldObject from the receiver's elements. If several of the
	elements are equal to oldObject, only one is removed. If no element is
	equal to oldObject, answer the result of evaluating anExceptionBlock.
	Otherwise, answer the argument, oldObject. ArrayedCollections cannot
	respond to this message."

	self subclassResponsibility
]

{ #category : #removing }
Collection >> removeAll [
	"Remove each element from the receiver and leave it empty.
	ArrayedCollections cannot respond to this message.
	There are two good reasons why a subclass should override this message:
	1) the subclass does not support being modified while being iterated
	2) the subclass provides a much faster way than iterating through each element"

	"#(10 20 10) asOrderedCollection removeAll >>> OrderedCollection new"

	self do: [:each | self remove: each]
]

{ #category : #removing }
Collection >> removeAll: aCollection [
	"Remove each element of aCollection from the receiver. If successful for
	each, answer aCollection. Otherwise create an error notification.
	ArrayedCollections cannot respond to this message."

	"(#(10 20 10 30 10) asOrderedCollection removeAll: #(10 10 20); yourself) >>> #(30 10) asOrderedCollection"

	aCollection == self ifTrue: [^self removeAll].
	aCollection do: [:each | self remove: each].
	^ aCollection
]

{ #category : #removing }
Collection >> removeAllFoundIn: aCollection [
	"Remove each element of aCollection which is present in the receiver
	from the receiver. Answer aCollection. No error is raised if an element
	isn't found. ArrayedCollections cannot respond to this message."

	"(#(10 20 10 10) asOrderedCollection removeAllFoundIn: #(10 20 30); yourself) >>> #(10 10) asOrderedCollection"

	aCollection do: [:each | self remove: each ifAbsent: []].
	^ aCollection
]

{ #category : #removing }
Collection >> removeAllSuchThat: aBlock [
	"Evaluate aBlock for each element and remove all that elements from
	the receiver for that aBlock evaluates to true.  Use a copy to enumerate
	collections whose order changes when an element is removed (i.e. Sets)."

	"(#(5 6 7 8) asOrderedCollection removeAllSuchThat: #even) >>> #(5 7) asOrderedCollection"

	self copy do: [:each | (aBlock value: each) ifTrue: [self remove: each]]
]

{ #category : #enumerating }
Collection >> select: aBlock [

	"Evaluate aBlock with each of the receiver's elements as the argument. Collect into a new collection like the receiver, only those elements for which aBlock evaluates to true. Answer the new collection."

	"(#(1 2 3 4 5) select: #even) >>> #(2 4)"

	"({1@2. 6@3. 2@ -1.} select: [:e| e x > e y]) >>> {(6@3). (2@ -1)}"

	"('heLlo wOrLd' select: #isUppercase) >>> 'LOL'"

	"((1 to: 10) select: #isPrime) >>> #(2 3 5 7)"

	"(#() select: [true]) >>> #()"

	| newCollection |
	newCollection := self copyEmpty.
	self do: [ :each |
		(aBlock value: each) ifTrue: [ newCollection add: each ] ].
	^ newCollection
]

{ #category : #enumerating }
Collection >> select: selectBlock thenCollect: collectBlock [
	"Utility method to improve readability."

	"(#(1 2 3 4 5) select: #even thenCollect: #negated) >>> #(-2 -4)"
	"('Hello, World!' select: #isLetter thenCollect: #uppercase) >>> 'HELLOWORLD'"

	^ (self select: selectBlock) collect: collectBlock
]

{ #category : #enumerating }
Collection >> select: selectBlock thenDo: doBlock [
	"Utility method to improve readability.
	Do not create the intermediate collection."

	"|s| s:=0. #(11 22 33) select: #odd thenDo: [:x|s:=s+x]. s >>> 44"

	self do: [: each |
		( selectBlock value: each )
			ifTrue: [ doBlock value: each ]
	]
]

{ #category : #accessing }
Collection >> size [
	"Answer how many elements the receiver contains."

	"#(10 20) size >>> 2"
	"#() size >>> 0"
	"#((())) size >>> 1"

	"'' size >>> 0"
	"'  ' size >>> 2"

	"(1 to:10) size >>> 10 "
	"(1 to:10 by:2) size >>> 5"
	"(10 to:1) size >>> 0"

	| tally |
	tally := 0.
	self do: [:each | tally := tally + 1].
	^ tally
]

{ #category : #sorting }
Collection >> sorted [
	"Return a new sequenceable collection which contains the same elements as self but its
elements are sorted"

	"#(3 1 4 2) sorted >>> #(1 2 3 4)"
	"'hello' sorted >>> 'ehllo'"
	"(10 to: 1 by: -2) sorted >>> (2 to: 10 by: 2)"

	^self asArray sorted
]

{ #category : #sorting }
Collection >> sorted: aSortBlockOrNil [
	"Return a new sequenceable collection which contains the same elements as self but its
elements are sorted by aSortBlockOrNil. The block should take two arguments and return true if
the first element should preceed the second one. If aSortBlock is nil then <= is used for
comparison."

	"(#(3 1 4 2) sorted: [:a :b| a>=b]) >>> #(4 3 2 1)"
	"('hello' sorted: [:a :b| a>=b]) >>> 'ollhe'"
	"((1 to: 10 by: 2) sorted: [:a :b| a>=b]) >>> #(9 7 5 3 1)"

	^self asArray sort: aSortBlockOrNil
]

{ #category : #printing }
Collection >> storeOn: aStream [
	"Refer to the comment in Object|storeOn:."

	| noneYet |
	aStream nextPutAll: '(('.
	aStream nextPutAll: self class name.
	aStream nextPutAll: ' new)'.
	noneYet := true.
	self do:
		[:each |
		noneYet
			ifTrue: [noneYet := false]
			ifFalse: [aStream nextPut: $;].
		aStream nextPutAll: ' add: '.
		aStream store: each].
	noneYet ifFalse: [aStream nextPutAll: '; yourself'].
	aStream nextPut: $)
]

{ #category : #'math functions' }
Collection >> sum: aBlock [
	"This is implemented using a variant of the normal inject:into: pattern.
	The reason for this is that it is not known whether we're in the normal
	number line, i.e. whether 0 is a good initial value for the sum.
	Consider a collection of measurement objects, 0 would be the unitless
	value and would not be appropriate to add with the unit-ed objects."

	"(#(1 -4 -10 1) sum: #abs) >>> 16"

	"({1@ -4. -10@1} sum: #abs) >>> (11@5)"

	"(#() sum: #abs) >>> 0"

	^ self sum: aBlock ifEmpty: [ 0 ]
]

{ #category : #'math functions' }
Collection >> sum: aBlock ifEmpty: anEmptySumBlock [
	"This is implemented using a variant of the normal inject:into: pattern.
	The reason for this is that it is not known whether we're in the normal
	number line, i.e. whether 0 is a good initial value for the sum.
	Consider a collection of measurement objects, 0 would be the unitless
	value and would not be appropriate to add with the unit-ed objects."
	| sum sample |

	^ self
		ifNotEmpty: [
			sample := aBlock value: self anyOne.
			sum := self inject: sample into: [ :previousValue :each |
				previousValue + (aBlock value: each) ].
			sum - sample ]
		ifEmpty: anEmptySumBlock
]

{ #category : #'math functions' }
Collection >> sumNumbers [
	"This is implemented using a variant of the normal inject:into: pattern
	that is specific to handling numbers. The receiver should include only numbers.

	Different from the sum implementation, the default value is zero. While sum is
	more general, sumNumbers is meant to support the most often encountered use case of
	dealing with numbers."

	"#(1 2 4) sumNumbers >>> 7"

	"#() sumNumbers >>> 0"

	^ self
		inject: 0
		into: [ :sum :each |  sum + each ]
]

{ #category : #'math functions' }
Collection >> sumNumbers: aBlock [
	"This is implemented using a variant of the normal inject:into: pattern
	that is specific to handling numbers. aBlock is expected to return a number
	for every element in the collection.

	Different from the sum: implementation, the default value is zero. While sum: is
	more general, sumNumbers: is meant to support the most often encountered use case of
	dealing with numbers."

	"(#(1 -2 4) sumNumbers: #abs) >>> 7"

	"(#() sumNumbers: #abs) >>> 0"

	^ self
		inject: 0
		into: [ :sum :each |  sum + (aBlock value: each) ]
]

{ #category : #enumerating }
Collection >> union: aCollection [

	"Answer the set theoretic union of two collections."

	"(#(1 2 3) union: #(4 5 6 2)) asSet >>> #(1 2 3 4 5 6) asSet"
	"('hello' union: 'world') >>> 'whlorde' "

	| set |
	set := self asSet addAll: aCollection; yourself.
	^ self species withAll: set asArray
]

{ #category : #enumerating }
Collection >> withIndexDo: elementAndIndexBlock [
	"Just like do: except that the iteration index supplies the second argument to the block"
	"Support collection enumeration with a counter, even though not ordered"

	"|s|s:=0. #(4 2 1) withIndexDo: [:e :i| s:=s + (e * (10 ** (i-1)))]. s >>> 124"

	"|a|a:= Array new: 3. #(10 20 30) withIndexDo: [:e :i| a at: 4-i put: e+1]. a >>> #(31 21 11)"

	| index |
	index := 0.
	self do: [:item | elementAndIndexBlock value: item value: (index := index+1)]
]

{ #category : #enumerating }
Collection >> | aCollection [

	"Return all the elements that appear in self or in aCollection.
	Alias of union:"

	"#(10 20 30) | (0 to: 15 by: 5) >>> #(0 15 5 30 20 10)"
	"'abc' | 'cbe' >>> 'bcea'"

	^ self union: aCollection
]
