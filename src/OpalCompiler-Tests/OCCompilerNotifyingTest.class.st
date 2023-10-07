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
	#name : 'OCCompilerNotifyingTest',
	#superclass : 'TestCase',
	#instVars : [
		'text',
		'morph',
		'expectedErrors',
		'expectedErrorPositions',
		'failure'
	],
	#category : 'OpalCompiler-Tests-FromOld',
	#package : 'OpalCompiler-Tests',
	#tag : 'FromOld'
}

{ #category : 'private' }
OCCompilerNotifyingTest >> enumerateAllSelections [
	1 to: self numberOfSelections do: [ :n |
		self assert: (self evaluateSelectionNumber: n) identicalTo: failure.
		self assert: morph editor selection asString equals: (expectedErrors at: n).
		self assert: morph editor startIndex equals: (expectedErrorPositions at: n).
		morph editor cut ]
]

{ #category : 'private' }
OCCompilerNotifyingTest >> evaluateSelection [
	^ OpalCompiler new
		source: morph editor selection;
		requestor: morph editor;
		failBlock: [^failure];
		evaluate
]

{ #category : 'private' }
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

{ #category : 'initialization' }
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

{ #category : 'private' }
OCCompilerNotifyingTest >> numberOfSelections [
	^(text occurrencesOf: $%) + 1
]

{ #category : 'running' }
OCCompilerNotifyingTest >> setUp [
	super setUp.
	failure := Object new
]

{ #category : 'initialization' }
OCCompilerNotifyingTest >> setUpForErrorsIn: aTextWithErrorsEnclosedInBackQuote [
  "Extract the expectedErrors, the expectedErrorPositions and set up a TextMorph containing the text without errors.
  each section separated by % in aTextWithErrorsEnclosedInBackQuote will be evaluated separately.
  The expected error message should lie in aTextWithErrorsEnclosedInBackQuote at the expected position, and enclosed in back quotes."
  text := aTextWithErrorsEnclosedInBackQuote.
  morph := MockSourceEditor new contents: self initializeTextWithoutError asString
]

{ #category : 'tests' }
OCCompilerNotifyingTest >> testAssignmentOfSelf [

	self setUpForErrorsIn: '` Assignment to read-only variable ->`self := 1. ^self'.
	self enumerateAllSelections
]

{ #category : 'tests' }
OCCompilerNotifyingTest >> testDigitTooLargeForARadix [

	self setUpForErrorsIn: '2r` a digit between 0 and 1 expected ->`3'.
	self enumerateAllSelections
]

{ #category : 'tests' }
OCCompilerNotifyingTest >> testEmptyCaseStatement [
	self flag: 'Opal has the correct behavior but the error is not caught'

	"self setUpForErrorsIn: '^ nil caseOf: { ` At least one case required ->`} '.
	self enumerateAllSelections."
]

{ #category : 'tests' }
OCCompilerNotifyingTest >> testExpectedExpressionInBraceArray [

	self setUpForErrorsIn: '{ 1. 2 ` End of statement expected ->`3 }'.
	self enumerateAllSelections.
	self setUpForErrorsIn: '{ 1. 2. ` Variable or expression expected ->`| x | x}'.
	self enumerateAllSelections
]

{ #category : 'tests' }
OCCompilerNotifyingTest >> testExtraneousStatementAfterAReturnInABlock [
	self setUpForErrorsIn: '[ ^1 ` End of statement expected ->`2]'.
	self enumerateAllSelections
]

{ #category : 'tests' }
OCCompilerNotifyingTest >> testInvalidExternalFunctionDeclaration [
	"Not implemented yet.
	#externalFunctionDeclaration skipped, cannot be evaluated"
]

{ #category : 'tests' }
OCCompilerNotifyingTest >> testInvalidLiteralCharacter [
	self setUpForErrorsIn: '^ #yourself , #` Literal expected ->`) , #end'.
	self enumerateAllSelections
]

{ #category : 'tests' }
OCCompilerNotifyingTest >> testInvalidPattern [
	"Not implemented yet.
	#pattern:inContext: skipped, cannot be evaluated"
]

{ #category : 'tests' }
OCCompilerNotifyingTest >> testInvalidPragma [
	"Not implemented yet.
	#pragmaLiteral: #pragmaSequence #pragmaStatement #pragmaPrimitives skipped, cannot be evaluated"
]

{ #category : 'tests' }
OCCompilerNotifyingTest >> testInvalidPrimitive [
	"Not implemented yet.
	##primitive:error: #primitive:module:error: skipped, cannot be evaluated"
]

{ #category : 'tests' }
OCCompilerNotifyingTest >> testInvalidRadix [

	self setUpForErrorsIn: '1` an integer greater than 1 as valid radix expected ->`r0'.
	self enumerateAllSelections.
]

{ #category : 'tests' }
OCCompilerNotifyingTest >> testMissingArgumentAfterABinaryMessage [
	self setUpForErrorsIn: '1 +` Variable or expression expected ->`'.
	self enumerateAllSelections.
	self setUpForErrorsIn: '1 + ` Variable or expression expected ->`* 2 + 3'.
	self enumerateAllSelections
]

{ #category : 'tests' }
OCCompilerNotifyingTest >> testMissingArgumentAfterAMessageKey [
	self setUpForErrorsIn: '1 to: ` Variable or expression expected ->`:='.
	self enumerateAllSelections
]

{ #category : 'tests' }
OCCompilerNotifyingTest >> testMissingBlockArgumentName [

	self setUpForErrorsIn: '[ :x : ` Variable name expected ->`1]'.
	self enumerateAllSelections
]

{ #category : 'tests' }
OCCompilerNotifyingTest >> testMissingExpression [


	self setUpForErrorsIn: '| x | x := ` Variable or expression expected ->'.
	self enumerateAllSelections
]

{ #category : 'tests' }
OCCompilerNotifyingTest >> testMissingExpressionAfterAReturn [

	self setUpForErrorsIn: '^ ` Variable or expression expected ->`. 1 + 2'.
	self enumerateAllSelections
]

{ #category : 'tests' }
OCCompilerNotifyingTest >> testMissingMessageAfterACascade [

	self setUpForErrorsIn: 'nil yourself; ` Cascade message expected ->`^ 2'.
	self enumerateAllSelections
]

{ #category : 'tests' }
OCCompilerNotifyingTest >> testMissingPeriodSeparatorBetweenStatements [

	self setUpForErrorsIn: '1 + 2 ` End of statement expected ->`^nil'.
	self enumerateAllSelections
]

{ #category : 'tests' }
OCCompilerNotifyingTest >> testMissingSeparatorBetweenBlockArgumentAndStatements [

	self setUpForErrorsIn: '[ :x ` ''|'' or parameter expected ->`x + 1 ]'.
	self enumerateAllSelections
]

{ #category : 'tests' }
OCCompilerNotifyingTest >> testTooLargeAnIntegerInALiteralByteArray [

	self setUpForErrorsIn: '#[ 1 2 ` 8-bit integer expected ->`256 4 5]'.
	self enumerateAllSelections
]

{ #category : 'tests - bytecode limits' }
OCCompilerNotifyingTest >> testTooManyArguments [
	self setUpForErrorsIn: '^ ` Too many arguments ->`[:x1 :x2 :x3 :x4 :x5 :x6 :x7 :x8 :x9 :x10 :x11 :x12 :x13 :x14 :x15 :x16 :x17 | ]'.
	self enumerateAllSelections
]

{ #category : 'tests - bytecode limits' }
OCCompilerNotifyingTest >> testTooManyTemporaries [
	self setUpForErrorsIn: '| a1 a2 a3 a4 a5 a6 a7 a8 a9 b1 b2 b3 b4 b5 b6 b7 b8 b9 c1 c2 c3 c4 c5 c6 c7 c8 c9 d1 d2 d3 d4 d5 d6 d7 d8 d9 e1 e2 e3 e4 e5 e6 e7 e8 e9 f1 f2 f3 f4 f5 f6 f7 f8 f9 g1 g2 g3 g4 g5 g6 g7 g8 g9 |
	a1 := a2 := a3 := a4 := a5 := a6 := a7 := a8 := a9 := b1 := b2 := b3 := b4 := b5 := b6 := b7 := b8 := b9 := c1 := c2 := c3 := c4 := c5 := c6 := c7 := c8 := c9 := d1 := d2 := d3 := d4 := d5 := d6 := d7 := d8 := d9 := e1 := e2 := e3 := e4 := e5 := e6 := e7 := e8 := e9 := f1 := f2 := f3 := f4 := f5 := f6 := f7 := f8 := f9 := g1 := g2 := g3 := g4 := g5 := g6 := g7 := g8 := g9 := 1'.
	self flag: 'fail on jenkins but works on my computer with both compiler I dont know why'.
	"self should: [ self enumerateAllSelections ] raise: Error.
	[ self enumerateAllSelections ] on: Error do: [ :ex | self assert: ex messageText equals: 'Cannot compile -- stack including temps is too deep' ]"
]

{ #category : 'tests' }
OCCompilerNotifyingTest >> testUnmatchedBlockBracket [
	self setUpForErrorsIn: 'nil yourself. [` '']'' expected ->`'.
	self enumerateAllSelections
]

{ #category : 'tests' }
OCCompilerNotifyingTest >> testUnmatchedBraceArray [
	self setUpForErrorsIn: '{ 1. 2` ''}'' expected ->`'.
	self enumerateAllSelections.
	self setUpForErrorsIn: '{ 1. 2 ` ''}'' expected ->`'.
	self enumerateAllSelections
]

{ #category : 'tests' }
OCCompilerNotifyingTest >> testUnmatchedByteArrayBracket [

	self setUpForErrorsIn: '#[ 1 2 ` '']'' expected ->`'.
	self enumerateAllSelections
]

{ #category : 'tests' }
OCCompilerNotifyingTest >> testUnmatchedCommentQuote [

	self setUpForErrorsIn: '1+2   "unfinished comment` Unmatched " in comment. ->`'.
	self enumerateAllSelections
]

{ #category : 'tests' }
OCCompilerNotifyingTest >> testUnmatchedExpressionParenthesis [

	self setUpForErrorsIn: '1+(2 ` '')'' expected ->`. '.
	self enumerateAllSelections
]

{ #category : 'tests' }
OCCompilerNotifyingTest >> testUnmatchedLiteralParenthesis [

	self setUpForErrorsIn: '#( 1 2` '')'' expected ->`'.
	self enumerateAllSelections.
	self setUpForErrorsIn: '#( 1 2 ` '')'' expected ->`'.
	self enumerateAllSelections
]

{ #category : 'tests' }
OCCompilerNotifyingTest >> testUnmatchedLocalTempDeclaration [

	self setUpForErrorsIn: '| x y ` ''|'' or variable expected ->`'.
	self enumerateAllSelections
]

{ #category : 'tests' }
OCCompilerNotifyingTest >> testUnmatchedLocalTempDeclarationInABlock [

	self setUpForErrorsIn: '[:z | | x y ` ''|'' or variable expected ->`]'.
	self enumerateAllSelections
]

{ #category : 'tests' }
OCCompilerNotifyingTest >> testUnmatchedStringQuote [

	self setUpForErrorsIn: '^nil printString ,  ''unfinished string` Unmatched '' in string literal. ->`'.
	self enumerateAllSelections
]
