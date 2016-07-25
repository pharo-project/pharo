tests - converting
testAsSmalltalkComment

	| exampleStrings  |
	exampleStrings := #(
		''
		' '
		'"'
		'""'
		'"""'
		'abc"abc'
		'abc""abc'
		'abc"hello"abc'
		'abc"'
		'"abc' ).

	"check that the result of scanning the comment is empty"
	exampleStrings do: [ :s |
		| tokens  |
		tokens := s asComment parseLiterals.
		self assert: (tokens isEmpty) ].

	"check that the result has the same non-quote characters as the original"
	exampleStrings do: [ :s |
		self assert: ( (s copyWithout: $") = (s asComment copyWithout: $"))].

	"finnaly, test for some common kinds of inputs"
	self assert: ( 'abc' asComment = '"abc"').
	self assert: ( 'abc"abc' asComment = '"abc""abc"').
	self assert: ('abc""abc' asComment = '"abc""abc"' ).
		