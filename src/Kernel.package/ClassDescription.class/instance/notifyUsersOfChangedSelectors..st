users notification
notifyUsersOfChangedSelectors: aCollection
	self traitUsers do: [:each |
		each noteChangedSelectors: aCollection]