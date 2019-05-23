Trait {
	#name : #TMorphicTableDataSourceCommons,
	#instVars : [
		'model'
	],
	#category : #'Spec-MorphicAdapters-Table'
}

{ #category : #accessing }
TMorphicTableDataSourceCommons >> headerColumn: column [

	column id ifNil: [ ^ nil ].
	^ FTCellMorph new 
		listCentering: #left;
		addMorph: column id asMorph;
		yourself
]

{ #category : #accessing }
TMorphicTableDataSourceCommons >> menuColumn: column row: rowIndex [
	| menuPresenter |

	menuPresenter := self model contextMenu.
	menuPresenter ifNil: [ ^ nil ].
	^ SpecBindings
		value: self model application adapterBindings
		during: [ menuPresenter value buildWithSpec ]
]

{ #category : #accessing }
TMorphicTableDataSourceCommons >> model [

	^ model
]

{ #category : #accessing }
TMorphicTableDataSourceCommons >> model: aTablePresenter [

	model := aTablePresenter
]
