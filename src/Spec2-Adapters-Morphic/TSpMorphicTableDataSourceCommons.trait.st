Trait {
	#name : #TSpMorphicTableDataSourceCommons,
	#instVars : [
		'model'
	],
	#category : #'Spec2-Adapters-Morphic-Table'
}

{ #category : #accessing }
TSpMorphicTableDataSourceCommons >> headerColumn: column [

	column id ifNil: [ ^ nil ].
	^ FTCellMorph new 
		listCentering: #left;
		addMorph: column id asMorph;
		yourself
]

{ #category : #accessing }
TSpMorphicTableDataSourceCommons >> menuColumn: column row: rowIndex [
	| menuPresenter |

	menuPresenter := self model contextMenu.
	menuPresenter ifNil: [ ^ nil ].
	^ SpBindings
		value: self model application adapterBindings
		during: [ menuPresenter value buildWithSpec ]
]

{ #category : #accessing }
TSpMorphicTableDataSourceCommons >> model [

	^ model
]

{ #category : #accessing }
TSpMorphicTableDataSourceCommons >> model: aTablePresenter [

	model := aTablePresenter
]
