private
addItem
	(list includes: item) ifTrue: [ ^ self ].
	list add: item 