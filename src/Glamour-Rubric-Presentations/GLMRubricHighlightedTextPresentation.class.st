"
|browser|
browser := GLMTabulator new.
browser row: #row.

browser transmit to: #row; andShow: [ :composite |
	composite custom: (GLMRubricHighlightedTextPresentation new
		editingMode: [GLMHighlighterTextRubEditingMode 
			withStylerDecorator: (RbEPersonDecorator new
				styler:  (GLMHighlighterTextParserStyler new
					parser: RbEPersonColorizer new))]) ].
	
browser openOn: '
person {	
	name	=	''Phil'';
	age = ''23'';	
}
person {}
'.
"
Class {
	#name : #GLMRubricHighlightedTextPresentation,
	#superclass : #GLMRubricTextPresentation,
	#instVars : [
		'editingMode',
		'font'
	],
	#category : #'Glamour-Rubric-Presentations'
}

{ #category : #accessing }
GLMRubricHighlightedTextPresentation >> editingMode [

	^ editingMode ifNil: [ nil ] ifNotNil: [ editingMode glamourValue: self ]
]

{ #category : #accessing }
GLMRubricHighlightedTextPresentation >> editingMode: aBlock [
	
	editingMode := aBlock
]

{ #category : #accessing }
GLMRubricHighlightedTextPresentation >> font [
	^ font
]

{ #category : #accessing }
GLMRubricHighlightedTextPresentation >> font: anObject [
	font := anObject
]

{ #category : #accessing }
GLMRubricHighlightedTextPresentation >> morphicFont [
	^ self font 
		ifNil: [ StandardFonts defaultFont  ]
		ifNotNil: [ :f | f glamourValue: self entity ]
]

{ #category : #accessing }
GLMRubricHighlightedTextPresentation >> renderGlamorouslyOn: aRenderer [
	self registerAnnouncements.
	^ aRenderer renderRubricHighlightedTextPresentation: self
]
