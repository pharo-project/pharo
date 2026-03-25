Extension { #name : 'Collection' }

{ #category : '*Deprecated13-Kernel' }
Collection >> asCommaString [

	"Return collection printed as 'a, b, c' "

	self deprecated: 'Use #join: instead'.

	^ String streamContents: [ :s | self asStringOn: s delimiter: ', ' ]
]

{ #category : '*Deprecated13-Kernel' }
Collection >> asCommaStringAnd [

	"Return collection printed as 'a, b and c' "
	
	self deprecated: 'Use #join: instead'.

	^String streamContents: [:s | self asStringOn: s delimiter: ', ' last: ' and ']
]

{ #category : '*Deprecated13-Kernel' }
Collection >> asStringOn: aStream delimiter: delimString [

	"Print elements on a stream separated
	with a delimiter String like: 'a, b, c'
	Uses #asString instead of #print:."
	
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
		
	self deprecated: 'Use #do:separatedBy: instead'.

	self do: [:elem | aStream print: elem] separatedBy: [aStream nextPutAll: delimString]
]

{ #category : '*Deprecated13-Kernel' }
Collection >> printOn: aStream delimiter: delimString last: lastDelimString [
	"Print elements on a stream separated
	with a delimiter between all the elements and with
	a special one before the last like: 'a, b and c'"

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
