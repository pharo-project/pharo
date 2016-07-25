private
render: anObject
	^ GTSpotterItemBrick new
		icon: (self itemIconFor: anObject);
		text: (self itemNameFor: anObject)