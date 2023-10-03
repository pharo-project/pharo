Extension { #name : 'FTTreeItem' }

{ #category : '*ProfilerUI' }
FTTreeItem >> pathForItemSuchThat: aFullBlockClosure [

	children do: [ :each |
		(each pathForItemSuchThat: aFullBlockClosure)
		ifNotEmpty: [ :path | ^ path ] ].
	^ #()
]
