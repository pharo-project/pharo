Class {
	#name : #SpMorphicComponentListAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#category : #'Spec2-Adapters-Morphic-ComponentList'
}

{ #category : #factory }
SpMorphicComponentListAdapter >> buildWidget [

	| datasource |
	datasource := SpComponentListFastTableDataSource new.
	datasource model: self model.
	widget := FTTableMorph new
		dataSource: datasource;
		hideColumnHeaders;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		setBalloonText: self help;
		yourself.
	self presenter whenPresentersChangedDo: [ widget refresh ].
	^ widget
]

{ #category : #accessing }
SpMorphicComponentListAdapter >> children [

	^ self rows collect: [ :row | | cell | 
		cell := row submorphs first.
		cell submorphs first ]
]

{ #category : #accessing }
SpMorphicComponentListAdapter >> rows [

	| tableContainer |
	tableContainer := self widget submorphs first.
	^ tableContainer submorphs
]
