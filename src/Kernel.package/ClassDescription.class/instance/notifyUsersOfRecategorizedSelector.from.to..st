users notification
notifyUsersOfRecategorizedSelector: element from: oldCategory to: newCategory
	self traitUsers do: [:each |
		each noteRecategorizedSelector: element from: oldCategory to: newCategory]