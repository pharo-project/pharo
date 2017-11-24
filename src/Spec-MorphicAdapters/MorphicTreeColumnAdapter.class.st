"
I am the bridge between a TreeColumnPresenter and a MorphTreeColumn.

Ialso add support for on the fly refresh
"
Class {
	#name : #MorphicTreeColumnAdapter,
	#superclass : #AbstractMorphicAdapter,
	#category : #'Spec-MorphicAdapters'
}

{ #category : #'widget API' }
MorphicTreeColumnAdapter >> argumentBlock [

	^ [ Array with: self model ]
]

{ #category : #factory }
MorphicTreeColumnAdapter >> buildWidget [
	^ SpecTreeColumn new
		resizable: self resizable;
		startWidth: self initialWidth;
		setHeaderButtonLabel: self headerLabel
			font: self headerFont
			icon: self headerIcon
			target: self
			actionSelector: self performHeaderActionSelector;
		rowMorphGetSelector: self displayBlock
]

{ #category : #'widget API' }
MorphicTreeColumnAdapter >> displayBlock [

	^ self model displayBlock
]

{ #category : #private }
MorphicTreeColumnAdapter >> displayBlockChanged [

	self widgetDo: [ :w | 
		w rowMorphGetSelector: self displayBlock.
		w container updateList ]
]

{ #category : #'widget API' }
MorphicTreeColumnAdapter >> headerAction [

	^ self model headerAction
]

{ #category : #'widget API' }
MorphicTreeColumnAdapter >> headerFont [

	^ self model headerFont
]

{ #category : #private }
MorphicTreeColumnAdapter >> headerFontChanged [
	
	self widgetDo: [ :w | 
		w header label: self headerLabel font: self headerFont.
		w container resizerChanged ]
]

{ #category : #'widget API' }
MorphicTreeColumnAdapter >> headerIcon [

	^ self model headerIcon
]

{ #category : #private }
MorphicTreeColumnAdapter >> headerIconChanged [
	
	self widgetDo: [ :w | 
		self headerIcon
			ifNil: [ w header removeIcon ]
			ifNotNil: [ w header icon: (ImageMorph new form: self headerIcon) ] ]
]

{ #category : #'widget API' }
MorphicTreeColumnAdapter >> headerLabel [

	^ self model headerLabel
]

{ #category : #private }
MorphicTreeColumnAdapter >> headerLabelChanged [
	
	self widgetDo: [ :w | 
		w header label: self headerLabel.
		w container resizerChanged ]
]

{ #category : #'widget API' }
MorphicTreeColumnAdapter >> initialWidth [

	^ self model initialWidth
]

{ #category : #'widget API' }
MorphicTreeColumnAdapter >> performHeaderAction [

	self headerAction cull: self model cull: self
]

{ #category : #'widget API' }
MorphicTreeColumnAdapter >> performHeaderActionSelector [

	^ self headerAction
		ifNil: [ nil ]
		ifNotNil: [ #performHeaderAction ]
]

{ #category : #'widget API' }
MorphicTreeColumnAdapter >> resizable [

	^ self model resizable
]

{ #category : #private }
MorphicTreeColumnAdapter >> resizableChanged [
	
	self widgetDo: [ :w | w resizable: self resizable ]
]

{ #category : #updating }
MorphicTreeColumnAdapter >> update: aSymbol [

	aSymbol == #displayBlockChanged ifTrue: [ ^ self displayBlockChanged ].
	aSymbol == #resizableChanged ifTrue: [ ^ self resizableChanged ].
	aSymbol == #headerLabelChanged ifTrue: [ ^ self headerLabelChanged ].
	aSymbol == #headerFontChanged ifTrue: [ ^ self headerFontChanged ].
	aSymbol == #headerIconChanged  ifTrue: [ ^ self headerIconChanged ].
	
	^ super update: aSymbol
]
