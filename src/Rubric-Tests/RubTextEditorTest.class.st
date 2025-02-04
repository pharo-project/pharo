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
RubTextEditorTest >> assertNextCharGroupFrom: actualFrom to: actualTo equals: expected [

	| actualIndexFrom actualIndexTo expectedIndex |
	actualIndexFrom := actualFrom indexOf: $¶.
	actualIndexTo := actualTo indexOf: $¶.
	expectedIndex := expected indexOf: $¶.

	actualIndexFrom to: actualIndexTo do: [ :i |
		self
			assert: (editor nextWord: i stopOnUpperCase: true)
			equals: expectedIndex ]
]

{ #category : 'asserting' }
RubTextEditorTest >> assertNextCharGroupOf: actual equals: expected [

	| actualIndex expectedIndex |
	actualIndex := actual indexOf: $¶.
	expectedIndex := expected indexOf: $¶.

	self
		assert: (editor nextWord: actualIndex stopOnUpperCase: true)
		equals: expectedIndex
]

{ #category : 'asserting' }
RubTextEditorTest >> assertPreviousCharGroupFrom: actualFrom to: actualTo equals: expected [

	| actualIndexFrom actualIndexTo expectedIndex |
	actualIndexFrom := actualFrom indexOf: $¶.
	actualIndexTo := actualTo indexOf: $¶.
	expectedIndex := expected indexOf: $¶.

	actualIndexFrom to: actualIndexTo do: [ :i |
		self
			assert: (editor previousWord: i stopOnUpperCase: true)
			equals: expectedIndex ]
]

{ #category : 'asserting' }
RubTextEditorTest >> assertPreviousCharGroupOf: actual equals: expected [

	| actualIndex expectedIndex |
	actualIndex := actual indexOf: $¶.
	expectedIndex := expected indexOf: $¶.

	self
		assert: (editor previousWord: actualIndex stopOnUpperCase: true)
		equals: expectedIndex
]

{ #category : 'running' }
RubTextEditorTest >> setUp [

	super setUp.
	editor := RubTextEditor forTextArea: RubEditingArea new.
	"Add text with a paragraph"
	string := 'Lorem ipsum '.
	editor addString: string
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

	| textSize |
	textSize := string size.
	self assert: (editor nextWord: -999) equals: 6. "Out of range means start of text"
	self assert: (editor nextWord: 0) equals: 6. "Out of range means start of text"

	1 to: 5 do: [ :i |
		"From:   |Lorem ipsum
		 To:     Lore|m ipsum
		 Should be: Lorem| ipsum"
		self assert: (editor nextWord: i) equals: 6 ].

	6 to: 11 do: [ :i |
		"From:   Lorem |ipsum
		 To:     Lorem ipsu|m
		 Should be: Lorem ipsum|"
		self assert: (editor nextWord: i) equals: 12 ].

	"There is a space after ipsum:"
	"Lorem ipsum| >> Lorem ipsum |"
	self assert: (editor nextWord: 12) equals: 13.

	self assert: (editor nextWord: 999) equals: textSize + 1. "Out of range"
]

{ #category : 'tests' }
RubTextEditorTest >> testNextWordAtEndOfLineGoesToStartOfFirstWordOfNextLine [

	editor addString: 'a
	a
a'.

	self assert: (editor nextWord: 2) equals: 4
]

{ #category : 'tests' }
RubTextEditorTest >> testNextWordAtEndOfLineGoesToStartOfNextLine [

	editor addString: 'a

a'.

	self assert: (editor nextWord: 2) equals: 3
]

{ #category : 'tests' }
RubTextEditorTest >> testNextWordAtLineWithSpacesGoesToEndOfCurrentLine [

	editor addString: 'a  

  a'.

	self assert: (editor nextWord: 2) equals: 4
]

{ #category : 'tests' }
RubTextEditorTest >> testNextWordAtStartOfEmptyLineGoesToStartOfNextLine [

	editor addString: 'a

a'.

	self assert: (editor nextWord: 3) equals: 4
]

{ #category : 'tests' }
RubTextEditorTest >> testNextWordAtStartOfFirstWordGoesToEndOfLine [

	editor addString: 'a

a'.

	self assert: (editor nextWord: 4) equals: 5
]

{ #category : 'tests' }
RubTextEditorTest >> testNextWordGoesToEndOfCurrentWord [

	editor addString: 'abc d'.

	self assert: (editor nextWord: 3) equals: 4
]

{ #category : 'tests' }
RubTextEditorTest >> testNextWordSkipsCurrentSpacesAndGoesToEndOfCurrentWord [

	editor addString: 'abc d'.

	self assert: (editor nextWord: 4) equals: 6
]

{ #category : 'tests' }
RubTextEditorTest >> testNextWordStopOnUpperCase [

	string := 'loRem ipSum'.
	editor addString: string.

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
		equals: 						'loRem ipSum¶'.
]

{ #category : 'tests' }
RubTextEditorTest >> testNextWordStopOnUpperCaseComplex [

	string := 'self loRem: [[ (1 + 2) // 100 ]]'.
	editor addString: string.

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
		equals: 						'self loRem: [[ (1 + 2) // 100 ]]¶'.
]

{ #category : 'tests' }
RubTextEditorTest >> testNextWordStopOnUpperCaseManyUppercases [

	string := 'AAAb'.
	editor addString: string.

	self
		assertNextCharGroupFrom:	'¶AAAb'
		to: 							'A¶AAb'
		equals: 						'AA¶Ab'.

	self
		assertNextCharGroupFrom:	'AA¶Ab'
		to: 							'AAA¶b'
		equals: 						'AAAb¶'.
]

{ #category : 'tests' }
RubTextEditorTest >> testNextWordStopOnUpperCaseWithSpecialCharacters [

	string := '^[:x|(''#eur'',$$asString)]'.
	editor addString: string.

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
		equals: 						'^[:x|(''#eur'',$$asString)]¶'.
]

{ #category : 'tests' }
RubTextEditorTest >> testNextWordStopOnUpperCaseWithSyntax [

	string := '^"''|[](){}$#<>=;:.'.
	editor addString: string.

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
		equals: 						'^"''|[](){}$#<>=;:.¶'.
]

{ #category : 'tests' }
RubTextEditorTest >> testPreviousWord [

	| textSize |
	textSize := 'Lorem ipsum ' size.
	self assert: (editor previousWord: -999) equals: 1. "Out of range"
	self assert: (editor previousWord: 0) equals: 1. "Out of range"

	1 to: 7 do: [ :i |
		"From:   |Lorem ipsum
		 To:     Lorem |ipsum
		 Should be: |Lorem ipsum"
		self assert: (editor previousWord: i) equals: 1 ].

	8 to: 12 do: [ :i |
		"From:   Lorem |ipsum
		 To:     Lorem ipsum|
		 Should be: Lorem |ipsum"
		self assert: (editor previousWord: i) equals: 7 ].

	self assert: (editor previousWord: 999) equals: 7. "Out of range"
]

{ #category : 'tests' }
RubTextEditorTest >> testPreviousWordAtEndOfFirstWordOfLineGoesToStartOfLine [

	editor addString: 'a

a'.

	self assert: (editor previousWord: 5) equals: 4
]

{ #category : 'tests' }
RubTextEditorTest >> testPreviousWordAtLineWithSpacesGoesToStartOfCurrentLine [

	editor addString: 'a

  a'.

	self assert: (editor previousWord: 6) equals: 4
]

{ #category : 'tests' }
RubTextEditorTest >> testPreviousWordAtStartOfLineGoesToEndOfLastWordOfPreviousLine [

	editor addString: 'a	
a'.

	self assert: (editor previousWord: 4) equals: 1
]

{ #category : 'tests' }
RubTextEditorTest >> testPreviousWordAtStartOfLineGoesToPreviousLine [

	editor addString: 'a

a'.

	self assert: (editor previousWord: 4) equals: 3
]

{ #category : 'tests' }
RubTextEditorTest >> testPreviousWordGoesToStartOfCurrentWord [

	editor addString: 'abc def'.

	self assert: (editor previousWord: 6) equals: 5
]

{ #category : 'tests' }
RubTextEditorTest >> testPreviousWordSkipsCurrentSpacesAndGoesToStartOfCurrentWord [

	editor addString: 'abc def'.

	self assert: (editor previousWord: 5) equals: 1
]

{ #category : 'tests' }
RubTextEditorTest >> testPreviousWordStopOnUpperCase [

    string := 'loRem ipSum'.
    editor addString: string.
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

	string := 'self loRem: [[ (1 + 2) // 100 ]]'.
	editor addString: string.

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
		equals: 								'¶self loRem: [[ (1 + 2) // 100 ]]'.
]

{ #category : 'tests' }
RubTextEditorTest >> testPreviousWordStopOnUpperCaseManyUpercases [

	string := 'AAAb'.
	editor addString: string.

	self
		assertPreviousCharGroupFrom: 	'AAAb¶'
		to: 							  		'AAA¶b'
		equals: 						  		'AA¶Ab'.

	self
		assertPreviousCharGroupFrom:	'AA¶Ab'
		to: 							  		'¶AAAb'
		equals: 						  		'¶AAAb'.	

]

{ #category : 'tests' }
RubTextEditorTest >> testPreviousWordStopOnUpperCaseManyUppercases [
	string := 'AAAb'.
	editor addString: string.

	self
		assertPreviousCharGroupFrom:	'AAAb¶'
		to: 									'AAA¶b'
		equals: 								'AA¶Ab'.

	self
		assertPreviousCharGroupFrom:	'AA¶Ab'
		to: 									'¶AAAb'
		equals: 								'¶AAAb'.
]

{ #category : 'tests' }
RubTextEditorTest >> testPreviousWordStopOnUpperCaseWithSpecialCharacters [

	string := '^[:x|(''#eur'',$$asString)]'.
	editor addString: string.

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
		equals: 								'¶^[:x|(''#eur'',$$asString)]'.
]

{ #category : 'tests' }
RubTextEditorTest >> testPreviousWordStopOnUpperCaseWithSyntax [

	string := '^"''|[](){}$#<>=;:.'.
	editor addString: string.

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
		equals: 								'¶^"''|[](){}$#<>=;:.'.
]

{ #category : 'test-selection' }
RubTextEditorTest >> testSelectFromBeginToEnd [

	editor addString: 'a

a'.

	editor selectMark: 1 point: 5.
	self assert: editor selection equals: 'a

a'.
]

{ #category : 'tests' }
RubTextEditorTest >> testSelectWord [

	string := '#Lorem #ipsum #dolor #sit #amet'.
	editor addString: string.

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
	self assert: editor selection equals: 'amet'.

]

{ #category : 'tests' }
RubTextEditorTest >> testSelectWordMarkPoint [

	| textSize |
	string := 'Lorem ipsum dolor sit amet'.
	editor addString: string.
	textSize := editor string size.

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
