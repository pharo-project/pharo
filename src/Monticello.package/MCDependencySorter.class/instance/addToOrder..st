private
addToOrder: anItem
	orderedItems add: anItem.
	anItem provisions do: [:ea | self addProvision: ea].