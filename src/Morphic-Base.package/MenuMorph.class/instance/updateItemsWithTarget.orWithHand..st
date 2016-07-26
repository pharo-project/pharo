menu
updateItemsWithTarget: aTarget orWithHand: aHand
	"re-target all existing items"
	self items do: 
			[:item | item target ifNotNil: [
			item target isHandMorph 
				ifTrue: [item target: aHand]
				ifFalse: [item target: aTarget] ] ]