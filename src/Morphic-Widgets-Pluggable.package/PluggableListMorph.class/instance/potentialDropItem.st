drag and drop
potentialDropItem
	"return the item that the most recent drop hovered over, or nil if there is no potential drop target"
	self potentialDropRow = 0 ifTrue: [ ^self ].
	^self getListItem: self potentialDropRow