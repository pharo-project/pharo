Extension { #name : #CmdRootMenuGroup }

{ #category : #'*Iceberg-TipUI' }
CmdRootMenuGroup >> buildIceToolbarOn: aToolbar [
	(contents sorted: [ :a :b | a order < b order ])
		select: [ :each | each activator canExecuteCommand ]
		thenDo: [ :each | aToolbar addItem: each buildIceToolbarItem ]
]
