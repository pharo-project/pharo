"
I am a Morphic renderer for a GLMFastListPresentation.

Description
--------------------

I use a GLMFastListDataSource to manage the data of the table.

"
Class {
	#name : #GLMMorphicFastListRenderer,
	#superclass : #GLMMorphicFTRenderer,
	#category : #Glamour-FastTable-Morphic-Renderer
}

{ #category : #initialization }
GLMMorphicFastListRenderer >> createDataSourceFrom: aPresentation [
	^ aPresentation allowSeparators
		ifTrue: [ GLMFastListOutlineDataSource new ]
		ifFalse: [ GLMFastListDataSource new ]
]
