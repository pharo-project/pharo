accessing
selectedItemOrItemsOrNil
	| theItems |
	theItems := self selectedNodeList collect: [ :each | each item ].
	^ theItems isEmpty ifTrue: [nil] ifFalse: [ theItems ]