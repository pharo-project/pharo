private
drop: someItems from: sourceView
	"Just add 'wanted' items"

	sourceView == self ifTrue: [ ^ self ]. "nothing to be done"
	
	self addWantedItems: someItems.
	
	browser refresh.