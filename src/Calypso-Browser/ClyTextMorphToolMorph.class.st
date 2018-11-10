"
I am a root of possible tools which affect text editors.
Instances are supposed to be placed in the status bar of the editor.
They should be created using following method: 

	ClyTextMorphTool of: aTextMorph
	
If subclass need to be notified about text changes it should implement changes subscription in #attachToTextMorph method.
By default it do nothing.

Internal Representation and Key Implementation Points.

    Instance Variables
	textMorph:		<Object>
"
Class {
	#name : #ClyTextMorphToolMorph,
	#superclass : #Morph,
	#instVars : [
		'textMorph'
	],
	#category : #'Calypso-Browser-TextEditors'
}

{ #category : #'instance creation' }
ClyTextMorphToolMorph class >> of: aTextMorph [
	^self new 
		textMorph: aTextMorph
]

{ #category : #controlling }
ClyTextMorphToolMorph >> attachToTextMorph [

]

{ #category : #initialization }
ClyTextMorphToolMorph >> defaultColor [
	^Color transparent
]

{ #category : #controlling }
ClyTextMorphToolMorph >> detachFromTextMorph [

]

{ #category : #initialization }
ClyTextMorphToolMorph >> initialize [
	super initialize.	
	self 
		changeTableLayout;
		vResizing: #shrinkWrap;
		hResizing: #shrinkWrap
]

{ #category : #controlling }
ClyTextMorphToolMorph >> outOfWorld: aWorld [
	super outOfWorld: aWorld.
	
	self detachFromTextMorph 
]

{ #category : #accessing }
ClyTextMorphToolMorph >> textArea [
	^ textMorph textArea
]

{ #category : #accessing }
ClyTextMorphToolMorph >> textMorph [
	^ textMorph
]

{ #category : #accessing }
ClyTextMorphToolMorph >> textMorph: aTextMorph [
	textMorph := aTextMorph.
	self attachToTextMorph 
]
