"
I'm a column for the table on a MorphicTableAdapter hierarchy.
I delegate most of my behavior to my model (which is a TableColumn* instance)
"
Class {
	#name : #SpMorphicTableColumn,
	#superclass : #FTColumn,
	#instVars : [
		'model'
	],
	#category : #'Spec2-Adapters-Morphic-Table'
}

{ #category : #'instance creation' }
SpMorphicTableColumn class >> on: anObject [

	^ self new model: anObject
]

{ #category : #accessing }
SpMorphicTableColumn >> id [ 

	^ self model title
]

{ #category : #testing }
SpMorphicTableColumn >> isSortable [

	^ self model isSortable
]

{ #category : #accessing }
SpMorphicTableColumn >> model [
	^ model
]

{ #category : #accessing }
SpMorphicTableColumn >> model: anObject [

	model := anObject.
	model isSortable ifTrue: [ 
		self sortableUsing: model sortFunction ]
]

{ #category : #accessing }
SpMorphicTableColumn >> readObject: anObject [ 

	^ self model readObject: anObject
]

{ #category : #accessing }
SpMorphicTableColumn >> sortFunction [

	^ self model sortFunction
]
