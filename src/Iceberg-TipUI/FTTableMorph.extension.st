Extension { #name : #FTTableMorph }

{ #category : #'*Iceberg-TipUI' }
FTTableMorph >> selectFirstVisibleRow [
	^ self selectRowIndex: (container exposedRows
		ifNotEmpty: [ :rows | rows keys first ]
		ifEmpty: [ 0 ]).
]

{ #category : #'*Iceberg-TipUI' }
FTTableMorph >> selectLastVisibleRow [
	^ self selectRowIndex: (container exposedRows
		ifNotEmpty: [ :rows | rows keys last ]
		ifEmpty: [ 0 ]).
]

{ #category : #'*Iceberg-TipUI' }
FTTableMorph >> useFunction: aFilterFunctionClass do: aBlock [ 
	function := aFilterFunctionClass table: self.
	aBlock cull: function cull: self
]
