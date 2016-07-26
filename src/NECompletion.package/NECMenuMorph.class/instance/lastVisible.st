private
lastVisible

	^ (self firstVisible + self height-1)  min: (self itemsCount).