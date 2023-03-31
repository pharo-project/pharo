"
A RubTextEditorTest is a test class for testing the behavior of RubTextEditor
"
Class {
	#name : #RubTextEditorTest,
	#superclass : #TestCase,
	#instVars : [
		'editor',
		'string'
	],
	#category : #'Rubric-Tests-Editing-Core'
}

{ #category : #running }
RubTextEditorTest >> setUp [

	super setUp.
	editor := RubTextEditor forTextArea: RubTextFieldArea new.
	"Add text with a paragraph"
	string := 'Lorem ipsum '.
	editor addString: string
]

{ #category : #tests }
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

{ #category : #tests }
RubTextEditorTest >> testPreviousWord [

	| textSize |
	textSize := 'Lorem ipsum ' size.
	self assert: (editor previousWord: -999) equals: 1. "Out of range"
	self assert: (editor previousWord: 0) equals: 1. "Out of range"

	1 to: 7 do: [ :i |
		"From:   |Lorem ipsum
		 To:     Lore|m ipsum
		 Should be: Lorem| ipsum"
		self assert: (editor previousWord: i) equals: 1 ].

	8 to: 13 do: [ :i |
		"From:   Lorem |ipsum
		 To:     Lorem ipsu|m
		 Should be: Lorem ipsum|"
		self assert: (editor previousWord: i) equals: 7 ].

	self assert: (editor previousWord: 999) equals: 7. "Out of range"
]

{ #category : #tests }
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
