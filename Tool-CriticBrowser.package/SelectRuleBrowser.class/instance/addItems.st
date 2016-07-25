api
addItems
	toSelectModel selectedItem
		ifNotNil:
			[ :item |
			item content isComposite
				ifFalse: [ selectedRules add: item content ]
				ifTrue: [ selectedRules addAll: item content rules ] ].
	self updateTree