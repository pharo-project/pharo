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

{ #category : 'running' }
RubTextEditorTest >> setUp [

	super setUp.
	editor := RubTextEditor forTextArea: RubTextFieldArea new.
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

	"Lorem| ipsum >> Lorem |ipsum"
	self assert: (editor nextWord: 6) equals: 7.

	7 to: 11 do: [ :i |
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
RubTextEditorTest >> testNextWordStopOnUpperCase [

	| textSize |
	string := 'loRem ipSum '.
	editor addString: string.
	textSize := string size.
	self assert: (editor nextWord: -999 stopOnUpperCase: true) equals: 3. "Out of range means start of text"
	self assert: (editor nextWord: 0 stopOnUpperCase: true) equals: 3. "Out of range means start of text"

	1 to: 3 do: [ :i |
		"From:   |loRem ipSum 
		 To:     lo|Rem ipSum 
		 Should be: lo|Rem ipSum "
		self assert: (editor nextWord: i stopOnUpperCase: true) equals: 3 ].

	4 to: 5 do: [ :i |
		"From:   loR|em ipSum 
		 To:     loRe|m ipSum 
		 Should be: loRem| ipSum "
		self assert: (editor nextWord: i stopOnUpperCase: true) equals: 6 ].


	"Lorem| ipsum >> Lorem |ipsum"
	self assert: (editor nextWord: 6 stopOnUpperCase: true) equals: 7.

	7 to: 8 do: [ :i |
		"From:   loRem |ipSum 
		 To:     loRem i|pSum 
		 Should be: loRem ip|Sum "
		self assert: (editor nextWord: i stopOnUpperCase: true) equals: 9 ].

	10 to: 11 do: [ :i |
		"From:   loRem ip|Sum 
		 To:     loRem ipSum| 
		 Should be: loRem ipSum| "
		self assert: (editor nextWord: i stopOnUpperCase: true) equals: 12 ].

	"There is a space after ipsum:"
	"Lorem ipsum| >> Lorem ipsum |"
	self assert: (editor nextWord: 12 stopOnUpperCase: true) equals: 13.

	self assert: (editor nextWord: 999 stopOnUpperCase: true) equals: textSize + 1. "Out of range"
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

	8 to: 13 do: [ :i |
		"From:   Lorem |ipsum
		 To:     Lorem ipsum|
		 Should be: Lorem |ipsum"
		self assert: (editor previousWord: i) equals: 7 ].

	self assert: (editor previousWord: 999) equals: 7. "Out of range"
]

{ #category : 'tests' }
RubTextEditorTest >> testPreviousWordStopOnUpperCase [

	| textSize |
	string := 'LoRem ipSum '.
	editor addString: string.
	textSize := string size.
	self assert: (editor previousWord: -999 stopOnUpperCase: true) equals: 1. "Out of range"
	self assert: (editor previousWord: 0 stopOnUpperCase: true) equals: 1. "Out of range"

	1 to: 3 do: [ :i |
		"From:   |LoRem ipSum 
		 To:     Lo|Rem ipSum 
		 Should be: |LoRem ipSum "
		self assert: (editor previousWord: i stopOnUpperCase: true) equals: 1 ].

	4 to: 7 do: [ :i |
		"From:   LoR|em ipSum 
		 To:     LoRem |ipSum 
		 Should be: Lo|Rem ipSum "
		self assert: (editor previousWord: i stopOnUpperCase: true) equals: 3 ].

	8 to: 9 do: [ :i |
		"From:   LoRem i|pSum 
		 To:     LoRem ip|Sum 
		 Should be: Lorem |ipsum"
		self assert: (editor previousWord: i stopOnUpperCase: true) equals: 7 ].

	10 to: 13 do: [ :i |
		"From:   LoRem ipS|um 
		 To:     LoRem ipSum|
		 Should be: LoRem ip|Sum"
		self assert: (editor previousWord: i stopOnUpperCase: true) equals: 9 ].

	self assert: (editor previousWord: 999 stopOnUpperCase: true) equals: 9. "Out of range"
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

	editor selectWordMark: 9 point: 12. "Lorem ipsum dolor sit amet >> Lorem [ipsum ]dolor sit amet "
	self assert: editor markIndex equals: 7.
	self assert: editor pointIndex equals: 13.

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
