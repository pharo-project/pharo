"
Allow the annotation display in class comment pane
"
Class {
	#name : #RubCommentAnnotationDisplayer,
	#superclass : #RubScrolledTextSideRuler,
	#instVars : [
		'lineAnnotation',
		'row',
		'wrappingPolicyMorph',
		'lineNumbersDisplayMorph',
		'extendCommentPolicyMorph'
	],
	#category : #'Nautilus-Rubric'
}

{ #category : #querying }
RubCommentAnnotationDisplayer class >> key [
	^ #commentAnnotation
]

{ #category : #accessing }
RubCommentAnnotationDisplayer >> backgroundColor [
	
	| clr |
	
	clr := self paragraphProvider backgroundColor.
	
	^clr luminance > 0.5 
		ifTrue: [ clr darker ]
		ifFalse: [ clr lighter]
]

{ #category : #'submorphs-accessing' }
RubCommentAnnotationDisplayer >> changeExtendCommentPolicy [
	self paragraphProvider model announcer announce: NautilusExtendCommentSwitch
]

{ #category : #'submorphs-accessing' }
RubCommentAnnotationDisplayer >> changeLineNumbersDisplay [
	self paragraphProvider lineNumbersRuler 
		ifNil: [ self paragraphProvider withLineNumbers ]
		ifNotNil: [ self paragraphProvider withoutLineNumbers ].
	self paragraphProvider manageLayout.
	self updateContents
]

{ #category : #'submorphs-accessing' }
RubCommentAnnotationDisplayer >> changeWrappingPolicy [
	self paragraphProvider wrapped 
		ifTrue: [ self paragraphProvider beNotWrapped  ]
		ifFalse: [ self paragraphProvider beWrapped ].
	self updateContents.
	self paragraphProvider changed
]

{ #category : #geometry }
RubCommentAnnotationDisplayer >> computedHeightFrom: aRectangle [
	^ row ifNil: [0] ifNotNil: [row height]
]

{ #category : #geometry }
RubCommentAnnotationDisplayer >> computedWidthFrom: aRectangle [
	^ aRectangle width
]

{ #category : #'submorphs-accessing' }
RubCommentAnnotationDisplayer >> extendCommentPolicyMorph [
	^ IconicButton new
		labelGraphic: (self iconNamed: #expandBoxIcon);
		color: Color transparent;
		borderWidth: 0
]

{ #category : #accessing }
RubCommentAnnotationDisplayer >> fontToUse [
	^ UITheme current textFont
]

{ #category : #'initialize-release' }
RubCommentAnnotationDisplayer >> initialize [
	super initialize.
	self side: #bottom

]

{ #category : #accessing }
RubCommentAnnotationDisplayer >> level [
	^ 0
]

{ #category : #'submorphs-accessing' }
RubCommentAnnotationDisplayer >> lineNumbersDisplayLabel [
	^ self paragraphProvider lineNumbersRuler  
		ifNil: [ '+L' ]
		ifNotNil: [ 'L' ]
]

{ #category : #'submorphs-accessing' }
RubCommentAnnotationDisplayer >> lineNumbersDisplayLabelMorph [
	^ StringMorph
		contents: self lineNumbersDisplayLabel
		font: self fontToUse
]

{ #category : #geometry }
RubCommentAnnotationDisplayer >> manageLayoutInBounds: aRectangle [
	| ret |
	ret := super manageLayoutInBounds: aRectangle.
	self updateContents.
	^ ret
]

{ #category : #'submorphs-accessing' }
RubCommentAnnotationDisplayer >> noteNewOwner: aMorph [
	super noteNewOwner: aMorph.
	self color: self backgroundColor.
	lineAnnotation := RubTextFieldArea new.
	lineAnnotation withoutAnyDecorator.
	lineAnnotation backgroundColor: Color transparent.
	lineAnnotation font: self fontToUse.
	lineAnnotation beReadOnly.
	lineAnnotation textColor: self theme textColor.
	wrappingPolicyMorph := self wrappingPolicyLabelMorph.
	lineNumbersDisplayMorph := self lineNumbersDisplayLabelMorph.
	extendCommentPolicyMorph := self extendCommentPolicyMorph.
	row := self theme
		newRowIn: self
		for:
			{(self spacer: 2).
			lineAnnotation.
			(self spacer: 2).
			extendCommentPolicyMorph.
			(self spacer: 2).
			wrappingPolicyMorph.
			(self spacer: 2).
			lineNumbersDisplayMorph.
			(self spacer: 2)}.
	wrappingPolicyMorph on: #mouseDown send: #changeWrappingPolicy to: self.
	lineNumbersDisplayMorph on: #mouseDown send: #changeLineNumbersDisplay to: self.
	extendCommentPolicyMorph on: #mouseDown send: #changeExtendCommentPolicy to: self.
	row color: Color transparent.
	row hResizing: #spaceFill.
	lineAnnotation hResizing: #spaceFill.
	self addMorph: row
]

{ #category : #'event handling' }
RubCommentAnnotationDisplayer >> selectionChanged [
	self updateContents.
	super selectionChanged
]

{ #category : #'submorphs-accessing' }
RubCommentAnnotationDisplayer >> spacer: anInteger [
	^ Morph new
		borderWidth: 0;
		color: Color transparent;
		extent: anInteger@3
]

{ #category : #accessing }
RubCommentAnnotationDisplayer >> textColor [
	^ self paragraphProvider textColor

]

{ #category : #'event handling' }
RubCommentAnnotationDisplayer >> textOfCharBlock: aCharacterBlock [
	| t l c |
	l := aCharacterBlock textLine.
	c := aCharacterBlock stringIndex - l first + 1.
	t := l lineNumber printString , '/' , self lines size printString
		, ' [', c printString.
	self paragraphProvider annotationRuler
		ifNotNil: [ :r | 
			| tabIdx |
			tabIdx := ((aCharacterBlock left + 1) // self paragraph tabWidth) + 1.
			t := t , ':' , tabIdx printString ].
	t := t, ']'.
	^ t
]

{ #category : #'event handling' }
RubCommentAnnotationDisplayer >> themeChanged [

	self color: self backgroundColor .
	
	self allMorphsDo: [ :morph | 
		(morph isKindOf: StringMorph) ifTrue: [ 
			morph color: self theme textColor	
		] 
	].

	super themeChanged
]

{ #category : #'event handling' }
RubCommentAnnotationDisplayer >> updateContents [
	| t |
	row ifNil: [ ^self ].
	self textArea selectionStart ifNil: [ ^self ].
	self lines ifNil: [ ^self ].
	t := self textOfCharBlock: self textArea selectionStart.
	self textArea selectionStop ~= self textArea selectionStart
		ifTrue: [ t := t , ' -- ' , (self textOfCharBlock: self textArea selectionStop) ].
	lineAnnotation
		beEditableWhile: [ 
			lineAnnotation setTextWith: t.
			self layoutChanged ].
	wrappingPolicyMorph contents: self wrappingPolicyLabel.
	lineNumbersDisplayMorph contents: self lineNumbersDisplayLabel.
	row position: self innerBounds topLeft.
	row width: self innerBounds width
]

{ #category : #'submorphs-accessing' }
RubCommentAnnotationDisplayer >> wrappingPolicyLabel [
	^ self paragraphProvider wrapped
				ifTrue: [ 'W' ]
				ifFalse: [ 'NW' ]
]

{ #category : #'submorphs-accessing' }
RubCommentAnnotationDisplayer >> wrappingPolicyLabelMorph [
	^ StringMorph
		contents: self wrappingPolicyLabel
		font: self fontToUse
]
