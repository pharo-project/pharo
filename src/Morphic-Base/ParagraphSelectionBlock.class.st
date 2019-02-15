"
A selection block in a text paragraph 
"
Class {
	#name : #ParagraphSelectionBlock,
	#superclass : #Object,
	#instVars : [
		'first',
		'last',
		'color'
	],
	#category : #'Morphic-Base-Text Support'
}

{ #category : #'instance creation' }
ParagraphSelectionBlock class >> first: firstCharBlock last: lastCharBlock color: aColor [
	^ self new first: firstCharBlock last: lastCharBlock color: aColor
]

{ #category : #displaying }
ParagraphSelectionBlock >> displayInLine: line on: aCanvas [ 
	"Display myself in the passed line."
	| startIdx stopIdx leftX rightX |
	startIdx := first stringIndex.
	stopIdx := last stringIndex.
	(stopIdx < line first or: [startIdx > (line last + 1)]) ifTrue: [^self].	"No selection on this line"
	(stopIdx = line first and: [last textLine ~= line]) ifTrue: [^self].	"Selection ends on line above"
	(startIdx = (line last + 1) and: [last textLine ~= line]) ifTrue: [^self].	"Selection begins on line below"
	leftX := (startIdx < line first 
		ifTrue: [line] 
		ifFalse: [first]) left.
	rightX := (stopIdx > (line last + 1) or: [stopIdx = (line last + 1) and: [last textLine ~= line]])  
				ifTrue: [line right]
				ifFalse: [last left].
	aCanvas fillRectangle: (leftX @ line top corner: rightX @ line bottom) color: color
]

{ #category : #accessing }
ParagraphSelectionBlock >> first [
	^ first
]

{ #category : #initialization }
ParagraphSelectionBlock >> first: firstCharBlock last: lastCharBlock color: aColor [
	first := firstCharBlock.
	last := lastCharBlock.
	color := aColor
]

{ #category : #accessing }
ParagraphSelectionBlock >> last [
	^ last
]
