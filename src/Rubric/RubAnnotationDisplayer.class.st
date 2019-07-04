"
I represent a bar with annotations for the text area, like editing mode, line numbers etc
"
Class {
	#name : #RubAnnotationDisplayer,
	#superclass : #RubScrolledTextSideRuler,
	#traits : 'TRubWithWrapPolicy + (TRubWithLineNumber - {#fontToUse})',
	#classTraits : 'TRubWithWrapPolicy classTrait + TRubWithLineNumber classTrait',
	#instVars : [
		'lineAnnotation',
		'row',
		'wrappingPolicyMorph',
		'editingModeMorph',
		'tabWidthMorph',
		'columnDisplayMorph',
		'lineNumbersDisplayMorph'
	],
	#category : #'Rubric-Editing-Widgets'
}

{ #category : #querying }
RubAnnotationDisplayer class >> key [
	^ #annotation
]

{ #category : #menus }
RubAnnotationDisplayer class >> tabWidthMenuOn: aBuilder [
	"Specify the menu used to select the tab width"

	<contextMenu>
	<RubTabWidthMenu>
	(6 to: 200 by: 6) do: [ :t | (aBuilder item: t asString asSymbol) action: [ :model | model chosenTabWidth: t ] ] 
]

{ #category : #accessing }
RubAnnotationDisplayer >> backgroundColor [
	^ self paragraphProvider backgroundColor darker
]

{ #category : #'submorphs-accessing' }
RubAnnotationDisplayer >> changeColumnDisplay [
	self paragraphProvider columnRuler
		ifNil: [ self paragraphProvider withColumns ]
		ifNotNil: [ self paragraphProvider withoutColumns ].
	self textArea changed.
	self updateContents
]

{ #category : #'submorphs-accessing' }
RubAnnotationDisplayer >> chooseEditingMode: anEvent [
	self modeListMenu invokeAt: anEvent position in: self world.
	self updateContents
]

{ #category : #'submorphs-accessing' }
RubAnnotationDisplayer >> chooseTabWidth: anEvent [
	self tabWidthMenu invokeAt: anEvent position in: self world
]

{ #category : #'submorphs-accessing' }
RubAnnotationDisplayer >> chosenTabWidth: anInteger [
	self paragraph tabWidth: anInteger.
	self updateContents
]

{ #category : #'submorphs-accessing' }
RubAnnotationDisplayer >> columnDisplayLabel [
	^ self paragraphProvider columnRuler 
		ifNil: [ '+C' ]
		ifNotNil: [ 'C' ]
]

{ #category : #'submorphs-accessing' }
RubAnnotationDisplayer >> columnDisplayLabelMorph [
	^ StringMorph
		contents: self columnDisplayLabel
		font: self fontToUse
]

{ #category : #geometry }
RubAnnotationDisplayer >> computedHeightFrom: aRectangle [
	^ row ifNil: [0] ifNotNil: [row height]
]

{ #category : #geometry }
RubAnnotationDisplayer >> computedWidthFrom: aRectangle [
	^ aRectangle width
]

{ #category : #accessing }
RubAnnotationDisplayer >> editingModeLabel [
	^ self paragraphProvider editingMode label
]

{ #category : #'submorphs-accessing' }
RubAnnotationDisplayer >> editingModeLabelMorph [
	^ StringMorph
		contents: self editingModeLabel
		font: self fontToUse
]

{ #category : #initialization }
RubAnnotationDisplayer >> initialize [
	super initialize.
	self side: #bottom.

]

{ #category : #accessing }
RubAnnotationDisplayer >> level [
	^ 1
]

{ #category : #geometry }
RubAnnotationDisplayer >> manageLayoutInBounds: aRectangle [
	| ret |
	ret := super manageLayoutInBounds: aRectangle.
	self updateContents.
	^ ret
]

{ #category : #accessing }
RubAnnotationDisplayer >> modeListMenu [
	"Answer the menu to be presented to select an editing mode"

	^ (PragmaMenuBuilder pragmaKeyword: #RubEditingModeMenu model: self) menu
]

{ #category : #'submorphs-accessing' }
RubAnnotationDisplayer >> noteNewOwner: aMorph [
	super noteNewOwner: aMorph.
	self color: self backgroundColor.
	lineAnnotation := RubTextFieldArea new.
	lineAnnotation withoutAnyDecorator.
	lineAnnotation backgroundColor: Color transparent.
	lineAnnotation font: self fontToUse.
	lineAnnotation beReadOnly.
	lineAnnotation textColor: self textColor.
	wrappingPolicyMorph := self wrappingPolicyLabelMorph.
	editingModeMorph := self editingModeLabelMorph.
	tabWidthMorph := self tabWidthLabelMorph.
	columnDisplayMorph := self columnDisplayLabelMorph.
	lineNumbersDisplayMorph := self lineNumbersDisplayLabelMorph.
	row := self theme
		newRowIn: self
		for:
			{(self spacer: 2).
			lineAnnotation.
			(self spacer: 2).
			tabWidthMorph.
			(self spacer: 2).
			editingModeMorph.
			(self spacer: 2).
			wrappingPolicyMorph.
			(self spacer: 2).
			lineNumbersDisplayMorph.
			(self spacer: 2).
			columnDisplayMorph.
			(self spacer: 2)}.
	wrappingPolicyMorph on: #mouseDown send: #changeWrappingPolicy to: self.
	tabWidthMorph on: #mouseDown send: #chooseTabWidth: to: self.
	editingModeMorph on: #mouseDown send: #chooseEditingMode: to: self.
	columnDisplayMorph on: #mouseDown send: #changeColumnDisplay to: self.
	lineNumbersDisplayMorph on: #mouseDown send: #changeLineNumbersDisplay to: self.
	row color: Color transparent.
	row hResizing: #spaceFill.
	lineAnnotation hResizing: #spaceFill.
	self addMorph: row
]

{ #category : #'event handling' }
RubAnnotationDisplayer >> selectionChanged [
	self updateContents.
	super selectionChanged
]

{ #category : #'submorphs-accessing' }
RubAnnotationDisplayer >> spacer: anInteger [
	^ Morph new
		borderWidth: 0;
		color: Color transparent;
		extent: anInteger@3
]

{ #category : #accessing }
RubAnnotationDisplayer >> tabWidth [
	^ self paragraph tabWidth
]

{ #category : #'submorphs-accessing' }
RubAnnotationDisplayer >> tabWidthLabel [
	^ 'Tab width: ', self tabWidth printString
]

{ #category : #'submorphs-accessing' }
RubAnnotationDisplayer >> tabWidthLabelMorph [
	^ StringMorph
		contents: self tabWidthLabel
		font: self fontToUse
]

{ #category : #accessing }
RubAnnotationDisplayer >> tabWidthMenu [
	"Answer the menu to be presented to select a tab width"

	^ (PragmaMenuBuilder pragmaKeyword: #RubTabWidthMenu model: self) menu
]

{ #category : #accessing }
RubAnnotationDisplayer >> textColor [
	^ self textArea lineNumbersTextColor muchDarker

]

{ #category : #'event handling' }
RubAnnotationDisplayer >> textOfCharBlock: aCharacterBlock [
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
RubAnnotationDisplayer >> updateContents [
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
	editingModeMorph contents: self editingModeLabel.
	tabWidthMorph contents: self tabWidthLabel.
	columnDisplayMorph contents: self columnDisplayLabel.
	lineNumbersDisplayMorph contents: self lineNumbersDisplayLabel.
	row position: self innerBounds topLeft.
	row width: self innerBounds width
]
