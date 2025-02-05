"
A RubTextEditorTest is a test class for testing the behavior of RubTextEditor
"
Class {
	#name : 'RubTextEditorTest',
	#superclass : 'RubAbstractTest',
	#category : 'Rubric-Tests-Editing-Core',
	#package : 'Rubric-Tests',
	#tag : 'Editing-Core'
}

{ #category : 'asserting' }
RubTextEditorTest >> assertNextCharGroupFrom: fromString to: toString equals: expected [

	| fromIndex toIndex expectedIndex |
	fromIndex := fromString indexOf: $¶.
	toIndex := toString indexOf: $¶.
	expectedIndex := expected indexOf: $¶.

	fromIndex to: toIndex do: [ :index |
		| actualIndex |
		actualIndex := editor nextWord: index stopOnUpperCase: true.

		self
			assert: actualIndex = expectedIndex
			description: [
			self describe: actualIndex from: index comparedTo: expected ] ]
]

{ #category : 'asserting' }
RubTextEditorTest >> assertNextCharGroupOf: fromString equals: expected [

	| fromIndex expectedIndex actualIndex |
	fromIndex := fromString indexOf: $¶.
	expectedIndex := expected indexOf: $¶.

	actualIndex := editor nextWord: fromIndex stopOnUpperCase: true.

	self
		assert: actualIndex = expectedIndex
		description: [
		self describe: actualIndex from: fromIndex comparedTo: expected ]
]

{ #category : 'asserting' }
RubTextEditorTest >> assertNextWordFrom: fromString to: toString equals: expected [

	| fromIndex toIndex expectedIndex |
	fromIndex := fromString indexOf: $¶.
	toIndex := toString indexOf: $¶.
	expectedIndex := expected indexOf: $¶.

	fromIndex to: toIndex do: [ :index |
		| actualIndex |
		actualIndex := editor nextWord: index.

		self
			assert: actualIndex = expectedIndex
			description: [
			self describe: actualIndex from: index comparedTo: expected ] ]
]

{ #category : 'asserting' }
RubTextEditorTest >> assertNextWordOf: fromString equals: expected [

	| fromIndex expectedIndex actualIndex |
	fromIndex := fromString indexOf: $¶.
	expectedIndex := expected indexOf: $¶.

	actualIndex := editor nextWord: fromIndex.

	self
		assert: actualIndex = expectedIndex
		description: [
		self describe: actualIndex from: fromIndex comparedTo: expected ]
]

{ #category : 'asserting' }
RubTextEditorTest >> assertPreviousCharGroupFrom: fromString to: toString equals: expected [

	| fromIndex toIndex expectedIndex |
	fromIndex := fromString indexOf: $¶.
	toIndex := toString indexOf: $¶.
	expectedIndex := expected indexOf: $¶.

	fromIndex to: toIndex by: -1 do: [ :index |
		| actualIndex |
		actualIndex := editor previousWord: index stopOnUpperCase: true.

		self
			assert: actualIndex = expectedIndex
			description: [
			self describe: actualIndex from: index comparedTo: expected ] ]
]

{ #category : 'asserting' }
RubTextEditorTest >> assertPreviousCharGroupOf: fromString equals: expected [

	| fromIndex expectedIndex actualIndex |
	fromIndex := fromString indexOf: $¶.
	expectedIndex := expected indexOf: $¶.

	actualIndex := editor previousWord: fromIndex stopOnUpperCase: true.

	self
		assert: actualIndex = expectedIndex
		description: [
		self describe: actualIndex from: fromIndex comparedTo: expected ]
]

{ #category : 'asserting' }
RubTextEditorTest >> assertPreviousWordFrom: fromString to: toString equals: expected [

	| fromIndex toIndex expectedIndex |
	fromIndex := fromString indexOf: $¶.
	toIndex := toString indexOf: $¶.
	expectedIndex := expected indexOf: $¶.

	fromIndex to: toIndex by: -1 do: [ :index |
		| actualIndex |
		actualIndex := editor previousWord: index.

		self
			assert: actualIndex = expectedIndex
			description: [
			self describe: actualIndex from: index comparedTo: expected ] ]
]

{ #category : 'asserting' }
RubTextEditorTest >> assertPreviousWordOf: fromString equals: expected [

	| fromIndex expectedIndex actualIndex |
	fromIndex := fromString indexOf: $¶.
	expectedIndex := expected indexOf: $¶.

	actualIndex := editor previousWord: fromIndex.

	self
		assert: actualIndex = expectedIndex
		description: [
		self describe: actualIndex from: fromIndex comparedTo: expected ]
]

{ #category : 'asserting' }
RubTextEditorTest >> describe: actualIndex from: fromIndex comparedTo: expected [

	^ String streamContents: [ :stream |
		  stream
			  nextPutAll: 'Got ''';
			  next: actualIndex - 1 putAll: string startingAt: 1;
			  nextPut: $¶;
			  next: string size putAll: string startingAt: actualIndex;
			  nextPutAll: ''' instead of ''';
			  nextPutAll: expected;
			  nextPutAll: ''' coming from ''';
			  next: fromIndex - 1 putAll: string startingAt: 1;
			  nextPut: $¶;
			  next: string size putAll: string startingAt: fromIndex;
			  nextPutAll: '''.' ]
]

{ #category : 'initialization' }
RubTextEditorTest >> setString: aString [

	string := aString.
	editor addString: string
]

{ #category : 'running' }
RubTextEditorTest >> setUp [

	super setUp.
	editor := RubTextEditor forTextArea: RubEditingArea new
]

{ #category : 'tests' }
RubTextEditorTest >> testLineIndentationStart [

	| starts |
	string := 'foo<r>  a b<r><t><t>a<t>b<r><r><r> <t><r> ' expandMacros.
	editor textArea privateText: string.
	self assert: editor string equals: string.

	starts := (1 to: string size + 1) collect: [ :i |
		          editor lineIndentationStart: i ].
	self
		assert: starts
		equals:
		#( 1 1 1 1 7 7 7 7 7 7 13 13 13 13 13 13 17 18 21 21 21 23 23 )
]

{ #category : 'tests' }
RubTextEditorTest >> testLineStart [

	| starts |
	string := 'foo<r>  a b<r><t><t>a<t>b<r><r><r> <t><r> ' expandMacros.
	editor textArea privateText: string.
	self assert: editor string equals: string.

	starts := (1 to: string size + 1) collect: [ :i |
		          editor lineStart: i ].
	self
		assert: starts
		equals:
		#( 1 1 1 1 5 5 5 5 5 5 11 11 11 11 11 11 17 18 19 19 19 22 22 )
]

{ #category : 'tests' }
RubTextEditorTest >> testNextWord [

	self setString: 'Lorem ipsum '.

	self assert: (editor nextWord: -999) equals: 6. "Out of range means start of text"
	self assert: (editor nextWord: 0) equals: 6. "Out of range means start of text"

	self
		assertNextWordFrom: '¶Lorem ipsum '
		to: 'Lore¶m ipsum '
		equals: 'Lorem¶ ipsum '.

	self
		assertNextWordFrom: 'Lorem¶ ipsum '
		to: 'Lorem ipsu¶m '
		equals: 'Lorem ipsum¶ '.

	self assertNextWordOf: 'Lorem ipsum¶ ' equals: 'Lorem ipsum ¶'.

	self assert: (editor nextWord: 999) equals: string size + 1 "Out of range means end of text"
]

{ #category : 'tests' }
RubTextEditorTest >> testNextWordAtEndOfLineGoesToStartOfFirstWordOfNextLine [

	self setString: 'a
	a
a'.

	self
		assertNextWordOf: 'a¶
	a
a'
		equals: 'a
	¶a
a'
]

{ #category : 'tests' }
RubTextEditorTest >> testNextWordAtEndOfLineGoesToStartOfNextLine [

	self setString: 'a

a'.

	self
		assertNextWordOf: 'a¶

a'
		equals: 'a
¶
a'
]

{ #category : 'tests' }
RubTextEditorTest >> testNextWordAtLineWithSpacesGoesToEndOfCurrentLine [

	self setString: 'a  

  a'. "two trailing spaces at end of first line"

	self
		assertNextWordOf: 'a¶  

  a'
		equals: 'a  ¶

  a'
]

{ #category : 'tests' }
RubTextEditorTest >> testNextWordAtStartOfEmptyLineGoesToStartOfNextLine [

	self setString: 'a

a'.

	self
		assertNextWordOf: 'a
¶
a'
		equals: 'a

¶a'
]

{ #category : 'tests' }
RubTextEditorTest >> testNextWordAtStartOfFirstWordGoesToEndOfLine [

	self setString: 'a

a'.

	self
		assertNextWordOf: 'a

¶a'
		equals: 'a

a¶'
]

{ #category : 'tests' }
RubTextEditorTest >> testNextWordGoesToEndOfCurrentWord [

	self setString: 'abc d'.

	self assertNextWordOf: 'ab¶c d' equals: 'abc¶ d'
]

{ #category : 'tests' }
RubTextEditorTest >> testNextWordManyUppercases [

	self setString: 'ABCdEFG'.

	self assertNextWordFrom:	'¶ABCdEFG'
	to:								'ABCdEFG¶'
	equals:						'ABCdEFG¶'
]

{ #category : 'tests' }
RubTextEditorTest >> testNextWordSkipsCurrentSpacesAndGoesToEndOfCurrentWord [

	self setString: 'abc d'.

	self assertNextWordOf: 'abc¶ d' equals: 'abc d¶'
]

{ #category : 'tests' }
RubTextEditorTest >> testNextWordStopOnUpperCase [

	self setString: 'loRem ipSum'.

	self assert: (editor nextWord: -999 stopOnUpperCase: true) equals: 3. "Out of range means end of text"
	self assert: (editor nextWord: 0 stopOnUpperCase: true) equals: 3. "Out of range means end of text"
	self assert: (editor nextWord: 999 stopOnUpperCase: true) equals: string size + 1. "Out of range"

	self
		assertNextCharGroupFrom:	'¶loRem ipSum'
		to: 							'l¶oRem ipSum'
		equals: 						'lo¶Rem ipSum'.

	self
		assertNextCharGroupFrom:	'lo¶Rem ipSum'
		to: 							'loRe¶m ipSum'
		equals: 						'loRem¶ ipSum'.

	self
		assertNextCharGroupFrom:	'loRem¶ ipSum'
		to: 							'loRem i¶pSum'
		equals: 						'loRem ip¶Sum'.

	self
		assertNextCharGroupFrom:	'loRem ip¶Sum'
		to: 							'loRem ipSum¶'
		equals: 						'loRem ipSum¶'
]

{ #category : 'tests' }
RubTextEditorTest >> testNextWordStopOnUpperCaseComplex [

	self setString: 'self loRem: [[ (1 + 2) // 100 ]]'.

	self
		assertNextCharGroupFrom:	'self lo¶Rem: [[ (1 + 2) // 100 ]]'
		to: 							'self loRe¶m: [[ (1 + 2) // 100 ]]'
		equals: 						'self loRem¶: [[ (1 + 2) // 100 ]]'.

	self
		assertNextCharGroupOf:	'self loRem¶: [[ (1 + 2) // 100 ]]'
		equals: 						'self loRem:¶ [[ (1 + 2) // 100 ]]'.

	self
		assertNextCharGroupFrom:	'self loRem:¶ [[ (1 + 2) // 100 ]]'
		to: 							'self loRem: [¶[ (1 + 2) // 100 ]]'
		equals: 						'self loRem: [[¶ (1 + 2) // 100 ]]'.

	self
		assertNextCharGroupFrom:	'self loRem: [[¶ (1 + 2) // 100 ]]'
		to: 							'self loRem: [[ ¶(1 + 2) // 100 ]]'
		equals: 						'self loRem: [[ (¶1 + 2) // 100 ]]'.

	self
		assertNextCharGroupOf:	'self loRem: [[ (¶1 + 2) // 100 ]]'
		equals: 						'self loRem: [[ (1¶ + 2) // 100 ]]'.

	self
		assertNextCharGroupFrom:	'self loRem: [[ (1¶ + 2) // 100 ]]'
		to: 							'self loRem: [[ (1 ¶+ 2) // 100 ]]'
		equals: 						'self loRem: [[ (1 +¶ 2) // 100 ]]'.

	self
		assertNextCharGroupFrom:	'self loRem: [[ (1 +¶ 2) // 100 ]]'
		to: 							'self loRem: [[ (1 + ¶2) // 100 ]]'
		equals: 						'self loRem: [[ (1 + 2¶) // 100 ]]'.

	self
		assertNextCharGroupOf:	'self loRem: [[ (1 + 2¶) // 100 ]]'
		equals: 						'self loRem: [[ (1 + 2)¶ // 100 ]]'.

	self
		assertNextCharGroupFrom:	'self loRem: [[ (1 + 2)¶ // 100 ]]'
		to: 							'self loRem: [[ (1 + 2) /¶/ 100 ]]'
		equals: 						'self loRem: [[ (1 + 2) //¶ 100 ]]'.

	self
		assertNextCharGroupFrom:	'self loRem: [[ (1 + 2) //¶ 100 ]]'
		to: 							'self loRem: [[ (1 + 2) // 10¶0 ]]'
		equals: 						'self loRem: [[ (1 + 2) // 100¶ ]]'.

	self
		assertNextCharGroupFrom:	'self loRem: [[ (1 + 2) // 100¶ ]]'
		to: 							'self loRem: [[ (1 + 2) // 100 ]¶]'
		equals: 						'self loRem: [[ (1 + 2) // 100 ]]¶'
]

{ #category : 'tests' }
RubTextEditorTest >> testNextWordStopOnUpperCaseManyUppercases [

	self setString: 'ABCdEFG'.

	self
		assertNextCharGroupFrom:	'¶ABCdEFG'
		to: 							'A¶BCdEFG'
		equals: 						'AB¶CdEFG'.

	self
		assertNextCharGroupFrom:	'AB¶CdEFG'
		to: 							'ABC¶dEFG'
		equals: 						'ABCd¶EFG'.

	self
		assertNextCharGroupFrom:	'ABCd¶EFG'
		to: 							'ABCdEFG¶'
		equals: 						'ABCdEFG¶'
]

{ #category : 'tests' }
RubTextEditorTest >> testNextWordStopOnUpperCaseWithSpecialCharacters [

	self setString: '^[:x|(''#eur'',$$asString)]'.

	self
		assertNextCharGroupFrom:	'¶^[:x|(''#eur'',$$asString)]'
		to: 							'^[¶:x|(''#eur'',$$asString)]'
		equals: 						'^[:¶x|(''#eur'',$$asString)]'.

	self
		assertNextCharGroupOf:	'^[:¶x|(''#eur'',$$asString)]'
		equals: 						'^[:x¶|(''#eur'',$$asString)]'.

	self
		assertNextCharGroupOf:	'^[:x¶|(''#eur'',$$asString)]'
		equals: 						'^[:x|¶(''#eur'',$$asString)]'.

	self
		assertNextCharGroupFrom:	'^[:x|¶(''#eur'',$$asString)]'
		to: 							'^[:x|(''¶#eur'',$$asString)]'
		equals: 						'^[:x|(''#¶eur'',$$asString)]'.

	self
		assertNextCharGroupFrom:	'^[:x|(''#¶eur'',$$asString)]'
		to: 							'^[:x|(''#eu¶r'',$$asString)]'
		equals: 						'^[:x|(''#eur¶'',$$asString)]'.

	self
		assertNextCharGroupOf:	'^[:x|(''#eur¶'',$$asString)]'
		equals: 						'^[:x|(''#eur''¶,$$asString)]'.

	self
		assertNextCharGroupOf:	'^[:x|(''#eur''¶,$$asString)]'
		equals: 						'^[:x|(''#eur'',¶$$asString)]'.

	self
		assertNextCharGroupFrom:	'^[:x|(''#eur'',¶$$asString)]'
		to: 							'^[:x|(''#eur'',$¶$asString)]'
		equals: 						'^[:x|(''#eur'',$$¶asString)]'.

	self
		assertNextCharGroupFrom:	'^[:x|(''#eur'',$$¶asString)]'
		to: 							'^[:x|(''#eur'',$$a¶sString)]'
		equals: 						'^[:x|(''#eur'',$$as¶String)]'.

	self
		assertNextCharGroupFrom:	'^[:x|(''#eur'',$$as¶String)]'
		to: 							'^[:x|(''#eur'',$$asStrin¶g)]'
		equals: 						'^[:x|(''#eur'',$$asString¶)]'.

	self
		assertNextCharGroupFrom:	'^[:x|(''#eur'',$$asString¶)]'
		to: 							'^[:x|(''#eur'',$$asString)¶]'
		equals: 						'^[:x|(''#eur'',$$asString)]¶'
]

{ #category : 'tests' }
RubTextEditorTest >> testNextWordStopOnUpperCaseWithSyntax [

	self setString: '^"''|[](){}$#<>=;:.'.

	self
		assertNextCharGroupFrom:	'¶^"''|[](){}$#<>=;:.'
		to: 							'^"¶''|[](){}$#<>=;:.'
		equals: 						'^"''¶|[](){}$#<>=;:.'.

	self
		assertNextCharGroupOf:	'^"''¶|[](){}$#<>=;:.'
		equals: 						'^"''|¶[](){}$#<>=;:.'.

	self
		assertNextCharGroupFrom:	'^"''|¶[](){}$#<>=;:.'
		to: 							'^"''|[](){}$¶#<>=;:.'
		equals: 						'^"''|[](){}$#¶<>=;:.'.

	self
		assertNextCharGroupFrom:	'^"''|[](){}$#¶<>=;:.'
		to: 							'^"''|[](){}$#<>¶=;:.'
		equals: 						'^"''|[](){}$#<>=¶;:.'.

	self
		assertNextCharGroupFrom:	'^"''|[](){}$#<>=¶;:.'
		to: 							'^"''|[](){}$#<>=;:.¶'
		equals: 						'^"''|[](){}$#<>=;:.¶'
]

{ #category : 'tests' }
RubTextEditorTest >> testPreviousWord [

	self setString: 'Lorem ipsum '.

	self assert: (editor previousWord: -999) equals: 1. "Out of range"
	self assert: (editor previousWord: 0) equals: 1. "Out of range"

	self
		assertPreviousWordFrom: 'Lorem ¶ipsum '
		to: '¶Lorem ipsum '
		equals: '¶Lorem ipsum '.

	self
		assertPreviousWordFrom: 'Lorem ipsum ¶'
		to: 'Lorem i¶psum '
		equals: 'Lorem ¶ipsum '.

	self
		assert: (editor previousWord: 999)
		equals: ('Lorem ¶ipsum ' indexOf: $¶) "Out of range"
]

{ #category : 'tests' }
RubTextEditorTest >> testPreviousWordAtEndOfFirstWordOfLineGoesToStartOfLine [

	self setString: 'a

a'.

	self
		assertPreviousWordOf: 'a

a¶'
		equals: 'a

¶a'
]

{ #category : 'tests' }
RubTextEditorTest >> testPreviousWordAtLineWithSpacesGoesToStartOfCurrentLine [

	self setString: 'a

  a'.

	self
		assertPreviousWordOf: 'a

  ¶a'
		equals: 'a

¶  a'
]

{ #category : 'tests' }
RubTextEditorTest >> testPreviousWordAtStartOfLineGoesToEndOfLastWordOfPreviousLine [

	self setString: 'a	
a'. "trailing tab at the end of the first line"

	self
		assertPreviousWordOf: 'a	
¶a'
		equals: 'a¶	
a'
]

{ #category : 'tests' }
RubTextEditorTest >> testPreviousWordAtStartOfLineGoesToPreviousLine [

	self setString: 'a

a'.

	self
		assertPreviousWordOf: 'a

¶a'
		equals: 'a
¶
a'
]

{ #category : 'tests' }
RubTextEditorTest >> testPreviousWordGoesToStartOfCurrentWord [

	self setString: 'abc def'.

	self assertPreviousWordOf: 'abc d¶ef' equals: 'abc ¶def'
]

{ #category : 'tests' }
RubTextEditorTest >> testPreviousWordManyUppercases [

	self setString: 'ABCdEFG'.

	self
		assertPreviousWordFrom:	'ABCdEFG¶'
		to: 							'¶ABCdEFG'
		equals: 						'¶ABCdEFG'
]

{ #category : 'tests' }
RubTextEditorTest >> testPreviousWordSkipsCurrentSpacesAndGoesToStartOfCurrentWord [

	self setString: 'abc def'.

	self assertPreviousWordOf: 'abc ¶def' equals: '¶abc def'
]

{ #category : 'tests' }
RubTextEditorTest >> testPreviousWordStopOnUpperCase [

	self setString: 'loRem ipSum'.

	self assert: (editor previousWord: -999 stopOnUpperCase: true) equals: 1. "Out of range means start of text"
	self assert: (editor previousWord: 0 stopOnUpperCase: true) equals: 1. "Out of range means start of text"

	self
		assertPreviousCharGroupFrom:	'loRem ipSum¶'
		to: 									'loRem ipS¶um'
		equals: 								'loRem ip¶Sum'.

	self
		assertPreviousCharGroupFrom:	'loRem ip¶Sum'
		to: 									'loRem i¶pSum'
		equals: 								'loRem ¶ipSum'.

	self
		assertPreviousCharGroupFrom:	'loRem ¶ipSum'
		to: 									'loR¶em ipSum'
		equals: 								'lo¶Rem ipSum'.

	self
		assertPreviousCharGroupFrom:	'lo¶Rem ipSum'
		to: 									'¶loRem ipSum'
		equals: 								'¶loRem ipSum'
]

{ #category : 'tests' }
RubTextEditorTest >> testPreviousWordStopOnUpperCaseComplex [

	self setString: 'self loRem: [[ (1 + 2) // 100 ]]'.

	self
		assertPreviousCharGroupFrom:	'self loRem: [[ (1 + 2) // 100 ]]¶'
		to: 									'self loRem: [[ (1 + 2) // 100 ]¶]'
		equals: 								'self loRem: [[ (1 + 2) // 100 ¶]]'.

	self
		assertPreviousCharGroupFrom:	'self loRem: [[ (1 + 2) // 100 ¶]]'
		to: 									'self loRem: [[ (1 + 2) // 1¶00 ]]'
		equals: 								'self loRem: [[ (1 + 2) // ¶100 ]]'.

	self
		assertPreviousCharGroupFrom:	'self loRem: [[ (1 + 2) // ¶100 ]]'
		to: 									'self loRem: [[ (1 + 2) /¶/ 100 ]]'
		equals: 								'self loRem: [[ (1 + 2) ¶// 100 ]]'.

	self
		assertPreviousCharGroupFrom:	'self loRem: [[ (1 + 2) ¶// 100 ]]'
		to: 									'self loRem: [[ (1 + 2)¶ // 100 ]]'
		equals: 								'self loRem: [[ (1 + 2¶) // 100 ]]'.

	self
		assertPreviousCharGroupOf:		'self loRem: [[ (1 + 2¶) // 100 ]]'
		equals: 								'self loRem: [[ (1 + ¶2) // 100 ]]'.

	self
		assertPreviousCharGroupFrom:	'self loRem: [[ (1 + ¶2) // 100 ]]'
		to: 									'self loRem: [[ (1 +¶ 2) // 100 ]]'
		equals: 								'self loRem: [[ (1 ¶+ 2) // 100 ]]'.

	self
		assertPreviousCharGroupFrom:	'self loRem: [[ (1 ¶+ 2) // 100 ]]'
		to: 									'self loRem: [[ (1¶ + 2) // 100 ]]'
		equals: 								'self loRem: [[ (¶1 + 2) // 100 ]]'.

	self
		assertPreviousCharGroupOf:		'self loRem: [[ (¶1 + 2) // 100 ]]'
		equals: 								'self loRem: [[ ¶(1 + 2) // 100 ]]'.

	self
		assertPreviousCharGroupFrom:	'self loRem: [[ ¶(1 + 2) // 100 ]]'
		to: 									'self loRem: [¶[ (1 + 2) // 100 ]]'
		equals: 								'self loRem: ¶[[ (1 + 2) // 100 ]]'.

	self
		assertPreviousCharGroupFrom:	'self loRem: ¶[[ (1 + 2) // 100 ]]'
		to: 									'self loRem:¶ [[ (1 + 2) // 100 ]]'
		equals: 								'self loRem¶: [[ (1 + 2) // 100 ]]'.

	self
		assertPreviousCharGroupFrom:	'self loRem¶: [[ (1 + 2) // 100 ]]'
		to: 									'self loR¶em: [[ (1 + 2) // 100 ]]'
		equals: 								'self lo¶Rem: [[ (1 + 2) // 100 ]]'.

	self
		assertPreviousCharGroupFrom:	'self lo¶Rem: [[ (1 + 2) // 100 ]]'
		to: 									'self l¶oRem: [[ (1 + 2) // 100 ]]'
		equals: 								'self ¶loRem: [[ (1 + 2) // 100 ]]'.

	self
		assertPreviousCharGroupFrom:	'self ¶loRem: [[ (1 + 2) // 100 ]]'
		to: 									'¶self loRem: [[ (1 + 2) // 100 ]]'
		equals: 								'¶self loRem: [[ (1 + 2) // 100 ]]'
]

{ #category : 'tests' }
RubTextEditorTest >> testPreviousWordStopOnUpperCaseManyUppercases [

	self setString: 'ABCdEFG'.

	self
		assertPreviousCharGroupFrom:	'ABCdEFG¶'
		to: 									'ABCdE¶FG'
		equals: 								'ABCd¶EFG'.

	self
		assertPreviousCharGroupFrom:	'ABCd¶EFG'
		to: 									'ABC¶dEFG'
		equals: 								'AB¶CdEFG'.

	self
		assertPreviousCharGroupFrom:	'AB¶CdEFG'
		to: 									'¶ABCdEFG'
		equals: 								'¶ABCdEFG'
]

{ #category : 'tests' }
RubTextEditorTest >> testPreviousWordStopOnUpperCaseWithSpecialCharacters [

	self setString: '^[:x|(''#eur'',$$asString)]'.

	self
		assertPreviousCharGroupFrom:	'^[:x|(''#eur'',$$asString)]¶'
		to: 									'^[:x|(''#eur'',$$asString)¶]'
		equals: 								'^[:x|(''#eur'',$$asString¶)]'.

	self
		assertPreviousCharGroupFrom:	'^[:x|(''#eur'',$$asString¶)]'
		to: 									'^[:x|(''#eur'',$$asS¶tring)]'
		equals: 								'^[:x|(''#eur'',$$as¶String)]'.

	self
		assertPreviousCharGroupFrom:	'^[:x|(''#eur'',$$as¶String)]'
		to: 									'^[:x|(''#eur'',$$a¶sString)]'
		equals: 								'^[:x|(''#eur'',$$¶asString)]'.

	self
		assertPreviousCharGroupFrom:	'^[:x|(''#eur'',$$¶asString)]'
		to: 									'^[:x|(''#eur'',$¶$asString)]'
		equals: 								'^[:x|(''#eur'',¶$$asString)]'.

	self
		assertPreviousCharGroupOf:		'^[:x|(''#eur'',¶$$asString)]'
		equals: 								'^[:x|(''#eur''¶,$$asString)]'.

	self
		assertPreviousCharGroupOf:		'^[:x|(''#eur''¶,$$asString)]'
		equals: 								'^[:x|(''#eur¶'',$$asString)]'.

	self
		assertPreviousCharGroupFrom:	'^[:x|(''#eur¶'',$$asString)]'
		to: 									'^[:x|(''#e¶ur'',$$asString)]'
		equals: 								'^[:x|(''#¶eur'',$$asString)]'.

	self
		assertPreviousCharGroupFrom:	'^[:x|(''#¶eur'',$$asString)]'
		to: 									'^[:x|(¶''#eur'',$$asString)]'
		equals: 								'^[:x|¶(''#eur'',$$asString)]'.

	self
		assertPreviousCharGroupOf:		'^[:x|¶(''#eur'',$$asString)]'
		equals: 								'^[:x¶|(''#eur'',$$asString)]'.

	self
		assertPreviousCharGroupOf:		'^[:x¶|(''#eur'',$$asString)]'
		equals: 								'^[:¶x|(''#eur'',$$asString)]'.

	self
		assertPreviousCharGroupFrom:	'^[:¶x|(''#eur'',$$asString)]'
		to: 									'¶^[:x|(''#eur'',$$asString)]'
		equals: 								'¶^[:x|(''#eur'',$$asString)]'
]

{ #category : 'tests' }
RubTextEditorTest >> testPreviousWordStopOnUpperCaseWithSyntax [

	self setString: '^"''|[](){}$#<>=;:.'.

	self
		assertPreviousCharGroupFrom:	'^"''|[](){}$#<>=;:.¶'
		to: 									'^"''|[](){}$#<>=;¶:.'
		equals: 								'^"''|[](){}$#<>=¶;:.'.

	self
		assertPreviousCharGroupFrom:	'^"''|[](){}$#<>=¶;:.'
		to: 									'^"''|[](){}$#<¶>=;:.'
		equals: 								'^"''|[](){}$#¶<>=;:.'.

	self
		assertPreviousCharGroupFrom:	'^"''|[](){}$#¶<>=;:.'
		to: 									'^"''|[¶](){}$#<>=;:.'
		equals: 								'^"''|¶[](){}$#<>=;:.'.

	self
		assertPreviousCharGroupOf:		'^"''|¶[](){}$#<>=;:.'
		equals: 								'^"''¶|[](){}$#<>=;:.'.

	self
		assertPreviousCharGroupFrom:	'^"''¶|[](){}$#<>=;:.'
		to: 									'¶^"''|[](){}$#<>=;:.'
		equals: 								'¶^"''|[](){}$#<>=;:.'
]

{ #category : 'tests' }
RubTextEditorTest >> testSelectFromBeginToEnd [

	self setString: 'a

a'.
	editor selectMark: 1 point: string size.

	self assert: editor selection equals: 'a

a'
]

{ #category : 'tests' }
RubTextEditorTest >> testSelectWord [

	self setString: '#Lorem #ipsum #dolor #sit #amet'.

	editor selectWordMark: 0 point: 0.
	editor selectWord.

	self assert: editor hasSelection.
	self assert: editor selection equals: 'Lorem'.

	editor selectWordMark: 2 point: 4. "Lorem ipsum dolor sit amet >> [Lorem ]ipsum dolor sit amet "
	editor selectWord.
	self assert: editor selection equals: 'Lorem'.

	editor selectWordMark: 9 point: 11. "Lorem ipsum dolor sit amet >> Lorem [ipsum] dolor sit amet "
	editor selectWord.
	self assert: editor selection equals: 'ipsum'.

	editor selectWordMark: 9 point: 12. "Lorem ipsum dolor sit amet >> Lorem [ipsum ]dolor sit amet "
	editor selectWord.
	self assert: editor selection equals: 'ipsum'.

	editor selectWordMark: 3 point: 24. "Lorem ipsum dolor sit amet >> [Lorem ipsum dolor sit amet ]"
	editor selectWord.
	self assert: editor selection equals: 'sit'.

	editor selectWordMark: 1 point: 26. "Lorem ipsum dolor sit amet >> [Lorem ipsum dolor sit amet ]"
	editor selectWord.
	self assert: editor selection equals: 'amet'.

	editor selectWordMark: 1 point: 1. "Lorem ipsum dolor sit amet >> [Lorem] ipsum dolor sit amet"
	editor selectWord.
	self assert: editor selection equals: 'Lorem'.

	editor selectWordMark: 26 point: 26. "Lorem ipsum dolor sit amet >> Lorem ipsum dolor sit [amet ]"
	editor selectWord.
	self assert: editor selection equals: 'amet'
]

{ #category : 'tests' }
RubTextEditorTest >> testSelectWordMarkPoint [

	self setString: 'Lorem ipsum dolor sit amet'.

	editor selectWordMark: 0 point: 0. "Lorem ipsum dolor sit amet >> [L]orem ipsum dolor sit amet "
	self assert: editor markIndex equals: 1.
	self assert: editor pointIndex equals: 6.

	editor selectWordMark: 2 point: 4. "Lorem ipsum dolor sit amet >> [Lorem ]ipsum dolor sit amet "
	self assert: editor markIndex equals: 1.
	self assert: editor pointIndex equals: 6.

	editor selectWordMark: 9 point: 11. "Lorem ipsum dolor sit amet >> Lorem [ipsum] dolor sit amet "
	self assert: editor markIndex equals: 7.
	self assert: editor pointIndex equals: 12.

	editor selectWordMark: 9 point: 12. "Lorem ipsum dolor sit amet >> Lorem [ipsum dolor] sit amet "
	self assert: editor markIndex equals: 7.
	self assert: editor pointIndex equals: 18.

	editor selectWordMark: 3 point: 24. "Lorem ipsum dolor sit amet >> [Lorem ipsum dolor sit amet ]"
	self assert: editor markIndex equals: 1.
	self assert: editor pointIndex equals: 26 + 1.

	editor selectWordMark: 1 point: 26. "Lorem ipsum dolor sit amet >> [Lorem ipsum dolor sit amet ]"
	self assert: editor markIndex equals: 1.
	self assert: editor pointIndex equals: 26 + 1.

	editor selectWordMark: 1 point: 1. "Lorem ipsum dolor sit amet >> [Lorem] ipsum dolor sit amet"
	self assert: editor markIndex equals: 1.
	self assert: editor pointIndex equals: 6.

	editor selectWordMark: 26 point: 26. "Lorem ipsum dolor sit amet >> Lorem ipsum dolor sit [amet ]"
	self assert: editor markIndex equals: 23.
	self assert: editor pointIndex equals: 26 + 1.

	editor selectWordMark: 999 point: 999. "Lorem ipsum dolor sit amet >> Lorem ipsum dolor sit [amet]"
	self assert: editor markIndex equals: 23.
	self assert: editor pointIndex equals: 26 + 1
]
