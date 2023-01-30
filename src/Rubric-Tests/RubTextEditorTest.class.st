"
A RubTextEditorTest is a test class for testing the behavior of RubTextEditor
"
Class {
	#name : #RubTextEditorTest,
	#superclass : #TestCase,
	#instVars : [
		'editor'
	],
	#category : #'Rubric-Tests-Editing-Core'
}

{ #category : #test }
RubTextEditorTest >> setUp [

	super setUp.
	editor := RubTextEditor forTextArea: RubTextFieldArea new.
	"Add text with a paragraph"
	editor addString: 'Lorem ipsum dolor sit amet'.
]

{ #category : #test }
RubTextEditorTest >> testNextWord [

	| textSize |
	textSize := 'Lorem ipsum dolor sit amet' size.
	self assert: (editor nextWord: -999) equals: 1. "Out of range"
	self assert: (editor nextWord: 0) equals: 1. "Out of range"
	self assert: (editor nextWord: 1) equals: 1. "L|orem ipsum >> L|orem ipsum"
	self assert: (editor nextWord: 2) equals: 6. "Lo|rem ipsum >> Lorem |ipsum"
	self assert: (editor nextWord: 4) equals: 6. "Lore|m ipsum >> Lorem |ipsum"
	self assert: (editor nextWord: 11) equals: 12. "Lorem ipsum| dolor >> Lorem ipsum |dolor"
	self assert: (editor nextWord: 18) equals: 18. "Lorem ipsum dolor |sit amet >> Lorem ipsum dolor |sit amet"
	self assert: (editor nextWord: (textSize - 2)) equals: textSize. "Lorem ipsum dolor sit am|et >> Lorem ipsum dolor sit amet|"
	self assert: (editor nextWord: (textSize - 1)) equals: textSize. "Lorem ipsum dolor sit ame|t >> Lorem ipsum dolor sit amet|"
	self assert: (editor nextWord: (textSize)) equals: textSize. "Lorem ipsum dolor sit amet| >> Lorem ipsum dolor sit amet|"
	self assert: (editor nextWord: (textSize + 1)) equals: textSize. "Out of range"
	self assert: (editor nextWord: 999) equals: textSize. "Out of range"
]

{ #category : #test }
RubTextEditorTest >> testPreviousWord [

	| textSize |
	textSize := 'Lorem ipsum dolor sit amet' size.
	self assert: (editor previousWord: 0) equals: 1. "Out of range"
	self assert: (editor previousWord: 1) equals: 1. "L|orem ipsum >> L|orem ipsum"
	self assert: (editor previousWord: 4) equals: 1. "Lore|m ipsum >> L|orem ipsum"
	self assert: (editor previousWord: 11) equals: 7. "Lorem ipsum| >> Lorem i|psum"
	self assert: (editor previousWord: 18) equals: 18. "Lorem ipsum dolor |sit amet >> Lorem ipsum dolor |sit amet"
	self assert: (editor previousWord: (textSize - 1)) equals: (textSize - 3). "Lorem ipsum dolor sit ame|t >> Lorem ipsum dolor sit a|met"
	self assert: (editor previousWord: (textSize)) equals: (textSize - 3). "Lorem ipsum dolor sit amet| >> Lorem ipsum dolor sit a|met"
	self assert: (editor previousWord: (textSize + 1)) equals: (textSize). "Out of range"
	self assert: (editor previousWord: 999) equals: (textSize). "Out of range"
]

{ #category : #test }
RubTextEditorTest >> testSelectWordMarkPoint [

	| textSize |
	textSize := editor string size.
	
	editor selectWordMark: 0 point: 0. "Lorem ipsum dolor sit amet >> [L]orem ipsum dolor sit amet "
	self assert: editor markIndex equals: 1.
	self assert: editor pointIndex equals: 2.	
	
	editor selectWordMark: 2 point: 4. "Lorem ipsum dolor sit amet >> [Lorem ]ipsum dolor sit amet "
	self assert: editor markIndex equals: 1.
	self assert: editor pointIndex equals: 7.	
	
	editor selectWordMark: 9 point: 12. "Lorem ipsum dolor sit amet >> Lorem [ipsum ]dolor sit amet "
	self assert: editor markIndex equals: 7.
	self assert: editor pointIndex equals: 13.	
	
	editor selectWordMark: 3 point: 24. "Lorem ipsum dolor sit amet >> [Lorem ipsum dolor sit amet ]"
	self assert: editor markIndex equals: 1.
	self assert: editor pointIndex equals: 26 + 1.	
	
	editor selectWordMark: 1 point: 26. "Lorem ipsum dolor sit amet >> [Lorem ipsum dolor sit amet ]"
	self assert: editor markIndex equals: 1.
	self assert: editor pointIndex equals: 26 + 1.	
	
	editor selectWordMark: 1 point: 1. "Lorem ipsum dolor sit amet >> [L]orem ipsum dolor sit amet"
	self assert: editor markIndex equals: 1.
	self assert: editor pointIndex equals: 2.	
	
	editor selectWordMark: 26 point: 26. "Lorem ipsum dolor sit amet >> Lorem ipsum dolor sit [amet ]"
	self assert: editor markIndex equals: 23.
	self assert: editor pointIndex equals: 26 + 1.	
	
	editor selectWordMark: 999 point: 999. "Lorem ipsum dolor sit amet >> Lorem ipsum dolor sit amet[]"
	self assert: editor markIndex equals: 26.
	self assert: editor pointIndex equals: 26 + 1.
]
