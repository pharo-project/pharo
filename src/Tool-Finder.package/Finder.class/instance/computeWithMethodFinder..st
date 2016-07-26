private-example
computeWithMethodFinder: aString
	"Compute the selectors for the single example of receiver and args, in the very top pane"

	| data result resultArray dataStrings methodFinder dataObjects temporarySearchResult statements |
	(aString includes: $.)
		ifFalse: [ ^ #() ].	"delete trailing period. This should be fixed in the Parser!"
	data := aString trimRight: [ :char | char isSeparator or: [ char = $. ] ].
	methodFinder := MethodFinder new.
	data := methodFinder cleanInputs: data.	"remove common mistakes"
	[ dataObjects := Smalltalk compiler evaluate: '{' , data , '}' ]
		on: SyntaxErrorNotification
		do: [ :e | 
			self inform: 'Syntax Error: ' , e errorMessage.
			^ #() ].	"#( data1 data2 result )"
	statements := (self class compiler parse: 'zort ' , data) body statements reject: [ :each | each isReturn ].
	dataStrings := statements
		collect: [ :node | 
			String
				streamContents: [ :strm | 
					node isMessage
						ifTrue: [ strm nextPut: $( ].
					strm << node formattedCode.
					node isMessage
						ifTrue: [ strm nextPut: $) ] ] ].
	dataObjects size < 2
		ifTrue: [ 
			self
				inform:
					'If you are giving an example of receiver, \args, and result, please put periods between the parts.\Otherwise just type one selector fragment'
						withCRs.
			^ #() ].
	dataObjects := Array with: dataObjects allButLast with: dataObjects last.	"#( (data1 data2) result )"
	result := methodFinder
		load: dataObjects;
		findMessage.
	(result first beginsWith: 'no single method')
		ifFalse: [ 
			temporarySearchResult := self testObjects: dataObjects strings: dataStrings.
			dataObjects := temporarySearchResult second.
			dataStrings := temporarySearchResult third ].
	resultArray := self listFromResult: result.
	resultArray isEmpty
		ifTrue: [ self inform: result first ].
	dataStrings size = (dataObjects first size + 1)
		ifTrue: [ 
			resultArray := resultArray
				collect: [ :expression | 
					| newExp |
					newExp := expression.
					dataObjects first
						withIndexDo: [ :lit :i | newExp := newExp copyReplaceAll: 'data' , i printString with: (dataStrings at: i) ].
					newExp , ' --> ' , dataStrings last ] ].
	^ resultArray