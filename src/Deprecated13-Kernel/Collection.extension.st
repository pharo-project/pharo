Extension { #name : 'Collection' }

{ #category : '*Deprecated13-Kernel' }
Collection >> asCommaString [

	"Return collection printed as 'a, b, c' "

	"#('a' 'b' 'c') asCommaString >>> 'a, b, c'"
	"#('a') asCommaString >>> 'a'"
	"#() asCommaString >>> ''"
	"'foo' asCommaString >>> 'f, o, o'"
	"(10 to: 25 by: 5) asCommaString >>> '10, 15, 20, 25'"

	self deprecated: 'Use #join: instead'.

	^ String streamContents: [ :s | self asStringOn: s delimiter: ', ' ]
]

{ #category : '*Deprecated13-Kernel' }
Collection >> asCommaStringAnd [

	"Return collection printed as 'a, b and c' "

	"#( 'a' 'b' 'c') asCommaStringAnd >>> 'a, b and c'"
	"#('a') asCommaStringAnd >>> 'a'"
	"#() asCommaStringAnd >>> ''"
	"'foo' asCommaStringAnd >>> 'f, o and o'"
	"(10 to: 25 by: 5) asCommaStringAnd >>> '10, 15, 20 and 25'"
	
	self deprecated: 'Use #join: instead'.

	^String streamContents: [:s | self asStringOn: s delimiter: ', ' last: ' and ']
]

{ #category : '*Deprecated13-Kernel' }
Collection >> asStringOn: aStream delimiter: delimString [

	"Print elements on a stream separated
	with a delimiter String like: 'a, b, c'
	Uses #asString instead of #print:."

	"(String streamContents: [:s|
		'abcd' asStringOn: s delimiter: '->'])
	>>> 'a->b->c->d'"
	
	self deprecated: 'Use #do:separatedBy: instead'.

	self
		do: [ :elem | aStream nextPutAll: elem asString ]
		separatedBy: [ aStream nextPutAll: delimString ]
]

{ #category : '*Deprecated13-Kernel' }
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
	
	self deprecated: 'Use #do:separatedBy: instead'.
	
	n := 1.
	sz := self size.
	self do: [:elem |
		n := n + 1.
		aStream nextPutAll: elem asString]
	separatedBy: [
		aStream nextPutAll: (n = sz ifTrue: [lastDelimString] ifFalse: [delimString])]
]

{ #category : '*Deprecated13-Kernel' }
Collection >> printOn: aStream delimiter: delimString [
	"Print elements on a stream separated
	with a delimiter String like: 'a, b, c'"

	"(String streamContents: [:s|
		{ 10. 'hello'. $x } printOn: s delimiter: ', '])
		>>> '10, ''hello'', $x'"
		
	self deprecated: 'Use #do:separatedBy: instead'.

	self do: [:elem | aStream print: elem] separatedBy: [aStream nextPutAll: delimString]
]

{ #category : '*Deprecated13-Kernel' }
Collection >> printOn: aStream delimiter: delimString last: lastDelimString [
	"Print elements on a stream separated
	with a delimiter between all the elements and with
	a special one before the last like: 'a, b and c'"

	"(String streamContents: [:s|
		{ 10. 'hello'. $x } printOn: s delimiter: ', ' last: ' & '])
		>>> '10, ''hello'' & $x'"

	"Note: Feel free to improve the code to detect the last element."

	| n sz |
	
	self deprecated: 'Use #do:separatedBy: instead'.
	
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
