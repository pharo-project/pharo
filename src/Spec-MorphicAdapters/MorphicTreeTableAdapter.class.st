"
I am the adapter used to realise a TreeTablePresenter.
I will create a tree table (using FTTableMorph), using the columns TableColumn* as definitions.

Is important notice here that most of the row/cell creation happens on MorphicTreeTableDataSource. Check there for some implementation details.
"
Class {
	#name : #MorphicTreeTableAdapter,
	#superclass : #MorphicTableAdapter,
	#category : #'Spec-MorphicAdapters-Table'
}

{ #category : #'private factory' }
MorphicTreeTableAdapter >> newDataSource [ 

	^ MorphicTreeTableDataSource new 
		rootItem: (self rootForItems: self model roots);
		childrenBlock: [ :data :item | self model childrenFor: data ];
		yourself
]

{ #category : #private }
MorphicTreeTableAdapter >> rootForItems: aCollection [

	^ FTRootItem new
		data: aCollection;
		yourself
]
