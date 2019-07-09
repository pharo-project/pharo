"
I am the bridge between a TreeColumnPresenter and a MorphTreeColumn.

Ialso add support for on the fly refresh
"
Class {
	#name : #SpMorphicTreeColumnAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#category : #'Spec2-Deprecated-Adapters'
}

{ #category : #deprecation }
SpMorphicTreeColumnAdapter class >> isDeprecated [ 

	^ true
]

{ #category : #'widget API' }
SpMorphicTreeColumnAdapter >> argumentBlock [

	^ [ Array with: self model ]
]

{ #category : #factory }
SpMorphicTreeColumnAdapter >> buildWidget [
	^ SpTreeColumn new
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
SpMorphicTreeColumnAdapter >> displayBlock [

	^ self model displayBlock
]

{ #category : #private }
SpMorphicTreeColumnAdapter >> displayBlockChanged [

	self widgetDo: [ :w | 
		w rowMorphGetSelector: self displayBlock.
		w container updateList ]
]

{ #category : #'widget API' }
SpMorphicTreeColumnAdapter >> headerAction [

	^ self model headerAction
]

{ #category : #'widget API' }
SpMorphicTreeColumnAdapter >> headerFont [

	^ self model headerFont
]

{ #category : #private }
SpMorphicTreeColumnAdapter >> headerFontChanged [
	
	self widgetDo: [ :w | 
		w header label: self headerLabel font: self headerFont.
		w container resizerChanged ]
]

{ #category : #'widget API' }
SpMorphicTreeColumnAdapter >> headerIcon [

	^ self model headerIcon
]

{ #category : #private }
SpMorphicTreeColumnAdapter >> headerIconChanged [
	
	self widgetDo: [ :w | 
		self headerIcon
			ifNil: [ w header removeIcon ]
			ifNotNil: [ w header icon: (ImageMorph new form: self headerIcon) ] ]
]

{ #category : #'widget API' }
SpMorphicTreeColumnAdapter >> headerLabel [

	^ self model headerLabel
]

{ #category : #private }
SpMorphicTreeColumnAdapter >> headerLabelChanged [
	
	self widgetDo: [ :w | 
		w header label: self headerLabel.
		w container resizerChanged ]
]

{ #category : #'widget API' }
SpMorphicTreeColumnAdapter >> initialWidth [

	^ self model initialWidth
]

{ #category : #'widget API' }
SpMorphicTreeColumnAdapter >> performHeaderAction [

	self headerAction cull: self model cull: self
]

{ #category : #'widget API' }
SpMorphicTreeColumnAdapter >> performHeaderActionSelector [

	^ self headerAction
		ifNil: [ nil ]
		ifNotNil: [ #performHeaderAction ]
]

{ #category : #'widget API' }
SpMorphicTreeColumnAdapter >> resizable [

	^ self model resizable
]

{ #category : #private }
SpMorphicTreeColumnAdapter >> resizableChanged [
	
	self widgetDo: [ :w | w resizable: self resizable ]
]

{ #category : #updating }
SpMorphicTreeColumnAdapter >> update: aSymbol [

	aSymbol == #displayBlockChanged ifTrue: [ ^ self displayBlockChanged ].
	aSymbol == #resizableChanged ifTrue: [ ^ self resizableChanged ].
	aSymbol == #headerLabelChanged ifTrue: [ ^ self headerLabelChanged ].
	aSymbol == #headerFontChanged ifTrue: [ ^ self headerFontChanged ].
	aSymbol == #headerIconChanged  ifTrue: [ ^ self headerIconChanged ].
	
	^ super update: aSymbol
]
