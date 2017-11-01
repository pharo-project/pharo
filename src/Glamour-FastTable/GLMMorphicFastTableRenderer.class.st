"
I am a Morphic renderer for a GLMFastTablePresentation.

Description
--------------------

I use a GLMFastTableDataSource to manage the data of the table.

"
Class {
	#name : #GLMMorphicFastTableRenderer,
	#superclass : #GLMMorphicFTRenderer,
	#category : 'Glamour-FastTable-Morphic-Renderer'
}

{ #category : #initialization }
GLMMorphicFastTableRenderer >> createDataSourceFrom: aPresentation [
	^ GLMFastTableDataSource new
]

{ #category : #initialization }
GLMMorphicFastTableRenderer >> specificTableMorphInitializiation [
	"Intialize first the columns and only then do the rest of the initialization."
	self specificTableMorphColumnsInitialization.
	super specificTableMorphInitializiation.
	
]
