"
A CompilerNotifyingTest is a TestCase for checking that Compiler/Parser notifications are inserted at the right place in a TextEditor.

Instance Variables
	expectedErrorPositions:		<Array of: Integer>
	expectedErrors:		<Array of: String>
	failure:		<Object>
	morph:		<TextMorph>
	text:		<String>

errorPositions
	- the position where error text should be inserted for each chunk of text evaluated

errors
	- the error text that should be inserted on evaluation of each chunk of text evaluated

failure
	- an object returned in case of evaluation error and whose identity can be uniquely recognized as a failure
	
morph
	- the Morph holding the text
	
text
	- the string containing all the chunks to be evaluated (separated by %)
	  and the expected error messages (`enclosed in back quotes`)
	  this text will be stripped of the error messages before being evaluated.


"
Class {
	#name : #OCCompilerNotifyingTest,
	#superclass : #TestCase,
	#instVars : [
		'text',
		'morph',
		'expectedErrors',
		'expectedErrorPositions',
		'failure'
	],
	#category : #'OpalCompiler-Tests-FromOld'
}

{ #category : #private }
OCCompilerNotifyingTest >> enumerateAllSelections [
	1 to: self numberOfSelections do: [ :n | 
		self assert: (self evaluateSelectionNumber: n) identicalTo: failure.
		self assert: morph editor selection asString equals: (expectedErrors at: n).
		self assert: (expectedErrorPositions at: n) equals: morph editor startIndex.
		morph editor cut ]
]

{ #category : #private }
OCCompilerNotifyingTest >> evaluateSelection [
	^ OpalCompiler new
		source: morph editor selectionAsStream;
		requestor: morph editor;
		failBlock: [^failure];
		evaluate

	
]

{ #category : #private }
OCCompilerNotifyingTest >> evaluateSelectionNumber: n [
	| i start stop |
	i := start := 1.
	[stop := morph text indexOf: $% startingAt: start + 1 ifAbsent: morph text size + 1.
	i = n]
		whileFalse:
			[i := i + 1.
			start := stop + 1].
	morph editor selectFrom: start to: stop - 1.
	^self evaluateSelection
	
]

{ #category : #initialization }
OCCompilerNotifyingTest >> initializeTextWithoutError [
	"Remove the errors from the text to be compiled and answer the text without errors.
	Meanwhile, collect the expected error messages and their expected position."

	| input output errorStream positionStream |
	input := text readStream.
	output := (String new: text size) writeStream.
	errorStream := (Array new: self numberOfSelections) writeStream.
	positionStream := (Array new: self numberOfSelections) writeStream.
	
	[output nextPutAll: (input upTo: $`).
	input atEnd]
		whileFalse:
			[positionStream nextPut: output position + 1.
			errorStream nextPut: (input upTo: $`)].
	expectedErrors := errorStream contents.
	expectedErrorPositions := positionStream contents.
	^output contents
]

{ #category : #private }
OCCompilerNotifyingTest >> numberOfSelections [
	^(text occurrencesOf: $%) + 1
]

{ #category : #running }
OCCompilerNotifyingTest >> setUp [
	super setUp.
	failure := Object new.
]

{ #category : #initialization }
OCCompilerNotifyingTest >> setUpForErrorsIn: aTextWithErrorsEnclosedInBackQuote [
  "Extract the expectedErrors, the expectedErrorPositions and set up a TextMorph containing the text without errors.
  each section separated by % in aTextWithErrorsEnclosedInBackQuote will be evaluated separately.
  The expected error message should lie in aTextWithErrorsEnclosedInBackQuote at the expected position, and enclosed in back quotes."
  text := aTextWithErrorsEnclosedInBackQuote.
  morph := MockSourceEditor new contents: self initializeTextWithoutError asString.
]

{ #category : #testing }
OCCompilerNotifyingTest >> testAssignmentOfSelf [

	self setUpForErrorsIn: '` Cannot store into ->`self := 1. ^self'.
	self enumerateAllSelections.
]

{ #category : #testing }
OCCompilerNotifyingTest >> testDigitTooLargeForARadix [

	self setUpForErrorsIn: '2r` Reading a number failed: a digit between 0 and 1 expected ->`3'.
	self enumerateAllSelections
]

{ #category : #testing }
OCCompilerNotifyingTest >> testEmptyCaseStatement [
	self flag: 'Opal has the correct behavior but the error is not caught'

	"self setUpForErrorsIn: '^ nil caseOf: { ` At least one case required ->`} '.
	self enumerateAllSelections."
]

{ #category : #testing }
OCCompilerNotifyingTest >> testExpectedExpressionInBraceArray [
	
	self setUpForErrorsIn: '{ 1. 2 ` End of statement list encountered ->`3 }'.
	self enumerateAllSelections.
	self setUpForErrorsIn: '{ 1. 2. ` Variable or expression expected ->`| x | x}'.
	self enumerateAllSelections.
]

{ #category : #testing }
OCCompilerNotifyingTest >> testExtraneousStatementAfterAReturnInABlock [
	self setUpForErrorsIn: '[ ^1 ` End of statement list encountered ->`2]'.
	self enumerateAllSelections.
]

{ #category : #testing }
OCCompilerNotifyingTest >> testInvalidExternalFunctionDeclaration [
	"Not implemented yet.
	#externalFunctionDeclaration skipped, cannot be evaluated"
]

{ #category : #testing }
OCCompilerNotifyingTest >> testInvalidLiteralCharacter [
	self setUpForErrorsIn: '^ #yourself , #` Expecting a literal type ->`) , #end'.
	self enumerateAllSelections.
]

{ #category : #testing }
OCCompilerNotifyingTest >> testInvalidPattern [
	"Not implemented yet.
	#pattern:inContext: skipped, cannot be evaluated"
]

{ #category : #testing }
OCCompilerNotifyingTest >> testInvalidPragma [
	"Not implemented yet.
	#pragmaLiteral: #pragmaSequence #pragmaStatement #pragmaPrimitives skipped, cannot be evaluated"
]

{ #category : #testing }
OCCompilerNotifyingTest >> testInvalidPrimitive [
	"Not implemented yet.
	##primitive:error: #primitive:module:error: skipped, cannot be evaluated"
]

{ #category : #testing }
OCCompilerNotifyingTest >> testInvalidRadix [

	self setUpForErrorsIn: '1` Reading a number failed: an integer greater than 1 as valid radix expected ->`r0'.
	self enumerateAllSelections.
]

{ #category : #testing }
OCCompilerNotifyingTest >> testMissingArgumentAfterABinaryMessage [
	self setUpForErrorsIn: '1 +` Variable or expression expected ->`'.
	self enumerateAllSelections.
	self setUpForErrorsIn: '1 + ` Variable or expression expected ->`* 2 + 3'.
	self enumerateAllSelections
]

{ #category : #testing }
OCCompilerNotifyingTest >> testMissingArgumentAfterAMessageKey [
	self setUpForErrorsIn: '1 to: ` Variable or expression expected ->`:='.
	self enumerateAllSelections
]

{ #category : #testing }
OCCompilerNotifyingTest >> testMissingBlockArgumentName [

	self setUpForErrorsIn: '[ :x : ` Variable name expected ->`1]'.
	self enumerateAllSelections
]

{ #category : #testing }
OCCompilerNotifyingTest >> testMissingExpression [

	
	self setUpForErrorsIn: '| x | x := ` Variable or expression expected ->'.
	self enumerateAllSelections
]

{ #category : #testing }
OCCompilerNotifyingTest >> testMissingExpressionAfterAReturn [

	self setUpForErrorsIn: '^ ` Variable or expression expected ->`. 1 + 2'.
	self enumerateAllSelections.
]

{ #category : #testing }
OCCompilerNotifyingTest >> testMissingMessageAfterACascade [
	
	self setUpForErrorsIn: 'nil yourself; ` Message expected ->`^ 2'.
	self enumerateAllSelections
]

{ #category : #testing }
OCCompilerNotifyingTest >> testMissingPeriodSeparatorBetweenStatements [

	self setUpForErrorsIn: '1 + 2 ` End of statement list encountered ->`^nil'.
	self enumerateAllSelections.
]

{ #category : #testing }
OCCompilerNotifyingTest >> testMissingSeparatorBetweenBlockArgumentAndStatements [
	
	self setUpForErrorsIn: '[ :x ` ''|'' expected ->`x + 1 ]'.
	self enumerateAllSelections
]

{ #category : #testing }
OCCompilerNotifyingTest >> testTooLargeAnIntegerInALiteralByteArray [

	self setUpForErrorsIn: '#[ 1 2 ` Expecting 8-bit integer ->`256 4 5]'.
	self enumerateAllSelections
]

{ #category : #'testing-byteCode limits' }
OCCompilerNotifyingTest >> testTooManyArguments [
	self setUpForErrorsIn: '^ ` Too many arguments ->`[:x1 :x2 :x3 :x4 :x5 :x6 :x7 :x8 :x9 :x10 :x11 :x12 :x13 :x14 :x15 :x16 :x17 | ]'.
	self enumerateAllSelections.
]

{ #category : #'testing-byteCode limits' }
OCCompilerNotifyingTest >> testTooManyLiterals [
	(thisContext method  encoderClass  = EncoderForSistaV1) ifTrue: [ ^self ].  "new bytecode does not have this limit"
	
	self setUpForErrorsIn: '{#(1). #(2). #(3). #(4). #(5). #(6). #(7). #(8). #(9). #(10). #(11). #(12). #(13). #(14). #(15). #(16). #(17). #(18). #(19). #(20). #(21). #(22). #(23). #(24). #(25). #(26). #(27). #(28). #(29). #(30). #(31). #(32). #(33). #(34). #(35). #(36). #(37). #(38). #(39). #(40). #(41). #(42). #(43). #(44). #(45). #(46). #(47). #(48). #(49). #(50). #(51). #(52). #(53). #(54). #(55). #(56). #(57). #(58). #(59). #(60). #(61). #(62). #(63). #(64). #(65). #(66). #(67). #(68). #(69). #(70). #(71). #(72). #(73). #(74). #(75). #(76). #(77). #(78). #(79). #(80). #(81). #(82). #(83). #(84). #(85). #(86). #(87). #(88). #(89). #(90). #(91). #(92). #(93). #(94). #(95). #(96). #(97). #(98). #(99). #(100). #(101). #(102). #(103). #(104). #(105). #(106). #(107). #(108). #(109). #(110). #(111). #(112). #(113). #(114). #(115). #(116). #(117). #(118). #(119). #(120). #(121). #(122). #(123). #(124). #(125). #(126). #(127). #(128). #(129). #(130). #(131). #(132). #(133). #(134). #(135). #(136). #(137). #(138). #(139). #(140). #(141). #(142). #(143). #(144). #(145). #(146). #(147). #(148). #(149). #(150). #(151). #(152). #(153). #(154). #(155). #(156). #(157). #(158). #(159). #(160). #(161). #(162). #(163). #(164). #(165). #(166). #(167). #(168). #(169). #(170). #(171). #(172). #(173). #(174). #(175). #(176). #(177). #(178). #(179). #(180). #(181). #(182). #(183). #(184). #(185). #(186). #(187). #(188). #(189). #(190). #(191). #(192). #(193). #(194). #(195). #(196). #(197). #(198). #(199). #(200). #(201). #(202). #(203). #(204). #(205). #(206). #(207). #(208). #(209). #(210). #(211). #(212). #(213). #(214). #(215). #(216). #(217). #(218). #(219). #(220). #(221). #(222). #(223). #(224). #(225). #(226). #(227). #(228). #(229). #(230). #(231). #(232). #(233). #(234). #(235). #(236). #(237). #(238). #(239). #(240). #(241). #(242). #(243). #(244). #(245). #(246). #(247). #(248). #(249). #(250). #(251). #(252). #(253). #(254). #(255). #(256). `More than 256 literals referenced. 
You must split or otherwise simplify this method.
The 257th literal is: ->`#(257)}'.
	self should: [ self enumerateAllSelections ] raise: Error.
	[ self enumerateAllSelections ]
		on: Error 
		do: [ :exc |
			exc class = Error.
			exc messageText = 'too many literals (>256)' ]
]

{ #category : #'testing-byteCode limits' }
OCCompilerNotifyingTest >> testTooManyTemporaries [
	self setUpForErrorsIn: '| a1 a2 a3 a4 a5 a6 a7 a8 a9 b1 b2 b3 b4 b5 b6 b7 b8 b9 c1 c2 c3 c4 c5 c6 c7 c8 c9 d1 d2 d3 d4 d5 d6 d7 d8 d9 e1 e2 e3 e4 e5 e6 e7 e8 e9 f1 f2 f3 f4 f5 f6 f7 f8 f9 g1 g2 g3 g4 g5 g6 g7 g8 g9 |
	a1 := a2 := a3 := a4 := a5 := a6 := a7 := a8 := a9 := b1 := b2 := b3 := b4 := b5 := b6 := b7 := b8 := b9 := c1 := c2 := c3 := c4 := c5 := c6 := c7 := c8 := c9 := d1 := d2 := d3 := d4 := d5 := d6 := d7 := d8 := d9 := e1 := e2 := e3 := e4 := e5 := e6 := e7 := e8 := e9 := f1 := f2 := f3 := f4 := f5 := f6 := f7 := f8 := f9 := g1 := g2 := g3 := g4 := g5 := g6 := g7 := g8 := g9 := 1'.
	self flag: 'fail on jenkins but works on my computer with both compiler I dont know why'.
	"self should: [ self enumerateAllSelections ] raise: Error.
	[ self enumerateAllSelections ] on: Error do: [ :ex | self assert: ex messageText equals: 'Cannot compile -- stack including temps is too deep' ]"
]

{ #category : #testing }
OCCompilerNotifyingTest >> testUnmatchedBlockBracket [
	self setUpForErrorsIn: 'nil yourself. [` '']'' expected ->`'.
	self enumerateAllSelections.
]

{ #category : #testing }
OCCompilerNotifyingTest >> testUnmatchedBraceArray [
	self setUpForErrorsIn: '{ 1. 2` expected } ->`'.
	self enumerateAllSelections.
	self setUpForErrorsIn: '{ 1. 2 ` expected } ->`'.
	self enumerateAllSelections
]

{ #category : #testing }
OCCompilerNotifyingTest >> testUnmatchedByteArrayBracket [

	self setUpForErrorsIn: '#[ 1 2 ` '']'' expected ->`'.
	self enumerateAllSelections
]

{ #category : #testing }
OCCompilerNotifyingTest >> testUnmatchedCommentQuote [

	self setUpForErrorsIn: '1+2   "unfinished comment` Unmatched " in comment. ->`'.
	self enumerateAllSelections
]

{ #category : #testing }
OCCompilerNotifyingTest >> testUnmatchedExpressionParenthesis [

	self setUpForErrorsIn: '1+(2 ` '')'' expected ->`. '.
	self enumerateAllSelections.
]

{ #category : #testing }
OCCompilerNotifyingTest >> testUnmatchedLiteralParenthesis [

	self setUpForErrorsIn: '#( 1 2` '')'' expected ->`'.
	self enumerateAllSelections.
	self setUpForErrorsIn: '#( 1 2 ` '')'' expected ->`'.
	self enumerateAllSelections.
]

{ #category : #testing }
OCCompilerNotifyingTest >> testUnmatchedLocalTempDeclaration [

	self setUpForErrorsIn: '| x y ` ''|'' expected ->`'.
	self enumerateAllSelections.
]

{ #category : #testing }
OCCompilerNotifyingTest >> testUnmatchedLocalTempDeclarationInABlock [

	self setUpForErrorsIn: '[:z | | x y ` ''|'' expected ->`]'.
	self enumerateAllSelections.
]

{ #category : #testing }
OCCompilerNotifyingTest >> testUnmatchedStringQuote [

	self setUpForErrorsIn: '^nil printString ,  ''unfinished string` Unmatched '' in string literal. ->`'.
	self enumerateAllSelections
]
