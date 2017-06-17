private
removeItem
	(list includes: item) ifFalse: [ ^ self ].
	list remove: item 