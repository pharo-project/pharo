Trait {
	#name : #SpTMorphicTableDataSourceCommons,
	#instVars : [
		'model'
	],
	#category : #'Spec2-Adapters-Morphic-Table'
}

{ #category : #accessing }
SpTMorphicTableDataSourceCommons >> headerColumn: column [
	| headerMorph |

	column id ifNil: [ ^ nil ].
	headerMorph := SpHeaderCellMorph new 
		addMorph: column id asMorph asReadOnlyMorph;
		yourself.
		
	column isSortable ifTrue: [ 
		headerMorph eventHandler: (MorphicEventHandler new 
			on: #click 
			send: #sortByColumn:event:morph:
			to: self
			withValue: column) ].

	^ headerMorph
]

{ #category : #accessing }
SpTMorphicTableDataSourceCommons >> menuColumn: column row: rowIndex [
	| menuPresenter |

	menuPresenter := self model contextMenu.
	menuPresenter ifNil: [ ^ nil ].
	^ SpBindings
		value: self model application adapterBindings
		during: [ menuPresenter value buildWithSpec ]
]

{ #category : #accessing }
SpTMorphicTableDataSourceCommons >> model [

	^ model
]

{ #category : #accessing }
SpTMorphicTableDataSourceCommons >> model: aTablePresenter [

	model := aTablePresenter
]

{ #category : #sorting }
SpTMorphicTableDataSourceCommons >> sortByColumn: aColumn event: anEvent morph: aMorph [

	aColumn sortDataSource: self
]
