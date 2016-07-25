accessing
itemsWithMissingRequirements
	| items |
	items := Set new.
	required do: [:ea | items addAll: ea].
	^ items
