"
I am a Morphic renderer for a GLMFastTreeTablePresentation. My main responsability is  to configure the table specific aspects of the presentation (columns, insets) and delegate to the appropriate data source.

Description
--------------------

I use a GLMFastTreeTableDataSource to manage the data of the tree with columns.
I will configure the renderer and the data source  based on a GLMFastTreeTablePresentation.

"
Class {
	#name : #GLMMorphicFastTreeWithColumnsRenderer,
	#superclass : #GLMMorphicFastTreeRenderer,
	#category : 'Glamour-FastTable-Morphic-Renderer'
}

{ #category : #initialization }
GLMMorphicFastTreeWithColumnsRenderer >> createDataSourceFrom: aPresentation [
	^ GLMFastTreeTableDataSource new
]

{ #category : #initialization }
GLMMorphicFastTreeWithColumnsRenderer >> specificTableMorphInitializiation [
	self specificTableMorphColumnsInitialization.
	super specificTableMorphInitializiation.
	

]
