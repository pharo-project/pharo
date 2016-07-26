registrations handling
arrangeRegistrations.
	self allMisplacedItems do: [:item |
		(self itemNamed: item parentName)
			ifNotNil: [:newOwner | item owner removeItem: item.
				item owner: newOwner.
				newOwner addItem: item]].
