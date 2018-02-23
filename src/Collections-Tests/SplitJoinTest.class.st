"
""Evaluate me to view the SplitJoin documentation:""

self showDocumentation

""CHANGES LOG:
- merged implementations by Keith Hodges (Join) and Damiena Pollet
  and Oscar Nierstrasz (RubyShards) into SplitJoin package
- moved all extension methods to *splitjoin method category
- merged all tests into SplitJoinTest
- fixed protocol in SequenceableCollection to splitOn: and joinUsing:
  and split: join: for splitters and joiners
- added Object>>joinTo: aStream and SequenceableCollection>>joinTo: aStream
  to support joining of either sequences or sequences of sequences
- prepared some documentation
- added systematic tests for all split/join cases
- added Object>>join:
- prepared split/join tests for all 16 cases
- prepares split+join tests for 4 standard cases
- reviewed/merged old tests
- changed splitjoin tests to use different joiner
- added separate test for split+join=id cases
- adapted documentation -- join result type is type of joiner or array or string
- fix split tests to check result asOrderedCollection
- added split tests for OrderedCollection and SortedCollection
- new join: method for OrderedCollection and SortedCollection
  (uses appendTo: in Object and SequenceableCollection)
- reviewed all split: implementations -- removed unnecessary helper methods
- check boundary conditions -- split on empty sequence ...
""
"
Class {
	#name : #SplitJoinTest,
	#superclass : #TestCase,
	#instVars : [
		'eg',
		'stringToSplit'
	],
	#category : #'Collections-Tests-SplitJoin'
}

{ #category : #documentation }
SplitJoinTest class >> documentation [
	<ignoreForCoverage>
	"self showDocumentation"
	
	^ '"This package provides functionality for splitting and joining strings similarly to that offered by Perl, Python and Ruby. In addition, split and join work for any kind of sequence, and offer the possibility to split strings using regular expressions.

Suppose object A is a SequenceableCollection of elements of type T (let''s call its type S[T]), and B is a splitter of type either T or S[T]. Then A splitOn: B yields an object C of type OC[S[T]] (OC=OrderedCollection). B can be either an element or a subsequence, so:"

	''abracadabra'' splitOn: $b.
	"-> an OrderedCollection(''a'' ''racada'' ''ra'')"

	''abracadabra'' splitOn: ''ca''.
	"-> an OrderedCollection(''abra'' ''dabra'')"

"A splitOn: B is the same as B split: A, so:"

	''ab'' split: ''abracadabra''.
	"-> an OrderedCollection('''' ''racad'' ''ra'')"

"joinUsing: and join: are the inverse of splitOn: and split:. You may use either of the forms:
	C joinUsing: B
or:
	B join: C
The result is normally the same type as the joiner. If the joiner (B) is of type S[T] and C is of type OC[S[T]] or Array[S[T]], the result is also of type S[T]. For example:"

	'' loves '' join: #(''john'' ''jane'' ''jack'').
	"-> ''john loves jane loves jack''"

"An object that is not a sequence can also be used as a joiner, in which case the final result is an array. Also the collection being joined may contain elements that are not sequences, in which case they are treated as singleton arrays."

	1 join: ''hello''.
	"-> #($h 1 $e 1 $l 1 $l 1 $o)"

"Note that in the case of characters and strings being used as joiners, the result is always a string."

"In general:
	((A splitOn: B) joinUsing: B) = A
or:
	(B join: (B split: A)) = A
so:"

	(''r'' join: (''r'' split: ''abracadabra'')) = ''abracadabra''.
	"->  true"

"Although the main application is for Strings, split and join will work for any kinds of SequenceableCollections, including Arrays and Intervals."

	[:n | n isPrime] split: (2 to: 20).
	"-> an OrderedCollection(#() #() #(4) #(6) #(8 9 10) #(12) #(14 15 16) #(18) #(20))"

"In addition, the following special cases are supported:

- split an S[T] using a block of type T->Boolean:"

	''abracadabra'' splitOn: [:x | ''bc'' includes: x].
	"-> an OrderedCollection(''a'' ''ra'' ''ada'' ''ra'')"

"- split a String using a regex:"

	''abracadabra'' splitOn: ''[bcdr]+'' asRegex.
	"-> an OrderedCollection(''a'' ''a'' ''a'' ''a'' ''a'')"

"- join all elements of an S[T] yielding another S[T]: '' ''- "

	''abracadabra'' joinUsing: '':''.
	" -> ''a:b:r:a:c:a:d:a:b:r:a''"

"Note that in these cases the rule that (B join: (B split: A)) = A does not hold, either because B maps to different elements, or because there is no A to start with."

	$: join: (1 to: 4).
	"-> ''1:2:3:4''"

"Finally, there is the convenient utility method joinUsing:last:, used as follows:"

	(1 to: 5) joinUsing: '', '' last: '' and ''.
	"-> ''1, 2, 3, 4 and 5''"

"This package merges and generalizes functionality from the Join package by Keith Hodges and the RubyShards package by Damien Pollet and Oscar Nierstrasz. "
'
]

{ #category : #sunitgui }
SplitJoinTest class >> packageNamesUnderTest [
	<ignoreForCoverage>
	^ #('SplitJoin')
]

{ #category : #running }
SplitJoinTest >> testJoinArrayUsingArray [
	self assert: ((1 to: 4) joinUsing: #(8 9))
		equals: #(1 8 9 2 8 9 3 8 9 4)
]

{ #category : #running }
SplitJoinTest >> testJoinArrayUsingChar [
	self assert: ((1 to: 4) joinUsing: $:)
		equals:  '1:2:3:4'
]

{ #category : #running }
SplitJoinTest >> testJoinArrayUsingObject [
	self assert: ((1 to: 4) joinUsing: 0)
		equals: #(1 0 2 0 3 0 4)
]

{ #category : #running }
SplitJoinTest >> testJoinArrayUsingOrderedCollection [
	self assert: ((1 to: 4) joinUsing: #(8 9) asOrderedCollection)
		equals: #(1 8 9 2 8 9 3 8 9 4) asOrderedCollection
]

{ #category : #running }
SplitJoinTest >> testJoinArrayUsingSortedCollection [
	self assert: ((1 to: 4) joinUsing: #(8 9) asSortedCollection)
		equals: #(1 8 9 2 8 9 3 8 9 4) asSortedCollection
]

{ #category : #running }
SplitJoinTest >> testJoinArrayUsingString [
	self assert: ((1 to: 4) joinUsing: '--')
		equals: '1--2--3--4'
]

{ #category : #running }
SplitJoinTest >> testJoinStringUsingChar [
	self assert: ('splitjoin' joinUsing: $-)
		equals: 's-p-l-i-t-j-o-i-n'
]

{ #category : #running }
SplitJoinTest >> testJoinStringUsingString [
	self assert: ('bda' joinUsing: 'an')
		equals: 'bandana'
]

{ #category : #running }
SplitJoinTest >> testJoinUsingLastOnArray [
	
	{	{  1. 2. 3 } 	-> '1, 2 and 3' .
		{  1. 2 } 	-> '1 and 2' .
		{  1 } 		-> '1' .
		{  }			-> ''.
	} 	asDictionary keysAndValuesDo: 
		[ 	:testCollection :resultString |
			self	 assert: ( testCollection joinUsing: ', ' last: ' and ') equals: resultString. 
		]

]

{ #category : #running }
SplitJoinTest >> testJoinUsingLastOnArrayOfStrings [
	self
		assert: (#('Pharo is modern' 'open source' 'highly portable' 'fast' 'full-featured' ) joinUsing: ', ' last: ' and ')
		equals: 'Pharo is modern, open source, highly portable, fast and full-featured'
]

{ #category : #running }
SplitJoinTest >> testSplitArrayOnBlock [
	self assert: ((1 to: 10) asArray splitOn: [:n| n even])
		equals: #(#(1) #(3) #(5) #(7) #(9) #()) asOrderedCollection
]

{ #category : #running }
SplitJoinTest >> testSplitArrayOnElement [
	self assert: ((1 to: 10) asArray splitOn: 4)
		equals: #(#(1 2 3) #(5 6 7 8 9 10)) asOrderedCollection

]

{ #category : #running }
SplitJoinTest >> testSplitArrayOnSequence [
	self assert: ((1 to: 10) asArray splitOn: (4 to: 5))
		equals: #(#(1 2 3) #(6 7 8 9 10)) asOrderedCollection

]

{ #category : #running }
SplitJoinTest >> testSplitJoinBoundaryCases [
	"Empty splitter, joiner or sequence."
	self assert: (#() join: (#() split: #())) equals: #().
	self assert: ('' join: ('' split: '')) equals: ''.
	self assert: ('' join: #('')) equals: ''. 

	"Overlapping splitters, or at end of sequence"
	self assert: ('an' join: ('an' split: 'banana')) equals: 'banana'.
	self assert: ('na' join: ('na' split: 'banana')) equals: 'banana'.
	self assert: ('ana' join: ('ana' split: 'banana')) equals: 'banana'.

]

{ #category : #running }
SplitJoinTest >> testSplitJoinIdentity [
	| array string |
	array := #(5 1 4 1 3 1 2 1).
	string := 'how now brown cow'.
	self assert: (1 join: (1 split: array)) equals: array.
	self assert: (#(1 3) join: (#(1 3) split: array)) equals: array.
	self assert: ($o join: ($o split: string)) equals: string.
	self assert: ('ow' join: ('ow' split: string)) equals: string.
]

{ #category : #running }
SplitJoinTest >> testSplitJoinOnElement [
	self assert: (0 join: (3 split: #(1 2 3 4 5)))
		equals: #(1 2 0 4 5)
]

{ #category : #running }
SplitJoinTest >> testSplitJoinOnSequence [
	self assert: (#(6 6 6) join: (#(3 4) split: #(1 2 3 4 5)))
		equals: #(1 2 6 6 6 5)
]

{ #category : #running }
SplitJoinTest >> testSplitJoinStringOnChar [
	self assert: ($: join: (Character space split: 'how now brown cow?'))
		equals: 'how:now:brown:cow?'
]

{ #category : #running }
SplitJoinTest >> testSplitJoinStringOnString [
	self assert: ('oo' join: ('ow' split: 'how now brown cow?'))
		equals: 'hoo noo broon coo?'
]

{ #category : #running }
SplitJoinTest >> testSplitOrderedCollectionOnElement [
	self assert: (((1 to: 10) asOrderedCollection) splitOn: 4)
		equals: {(1 to: 3) asOrderedCollection . (5 to: 10) asOrderedCollection} asOrderedCollection.

]

{ #category : #running }
SplitJoinTest >> testSplitOrderedCollectionOnOrderedCollection [
	self assert: (((1 to: 10) asOrderedCollection) splitOn: ((4 to: 5) asOrderedCollection))
		equals: {(1 to: 3) asOrderedCollection . (6 to: 10) asOrderedCollection} asOrderedCollection.

]

{ #category : #running }
SplitJoinTest >> testSplitSortedCollectionOnElement [
	self assert: (((1 to: 10) asSortedCollection) splitOn: 4)
		equals: {(1 to: 3) asSortedCollection . (5 to: 10) asSortedCollection} asOrderedCollection.

]

{ #category : #running }
SplitJoinTest >> testSplitSortedCollectionOnSortedCollection [
	self assert: (((1 to: 10) asSortedCollection) splitOn: ((4 to: 5) asSortedCollection))
		equals: {(1 to: 3) asSortedCollection . (6 to: 10) asSortedCollection} asOrderedCollection.


]

{ #category : #running }
SplitJoinTest >> testSplitStringOnBlock [
	self assert: ('foobar' splitOn: [:ch | 'aeiou' includes: ch])
		equals: #('f' '' 'b' 'r') asOrderedCollection
]

{ #category : #running }
SplitJoinTest >> testSplitStringOnChar [
	self assert: ('does eat oats and lambs eat oats' splitOn: Character space)
		equals: #('does' 'eat' 'oats' 'and' 'lambs' 'eat' 'oats') asOrderedCollection
]

{ #category : #running }
SplitJoinTest >> testSplitStringOnSubstring [
	self assert: ('the banana man can can bananas' splitOn: 'an')
		equals: #('the b' '' 'a m' ' c' ' c' ' b' '' 'as') asOrderedCollection
]
