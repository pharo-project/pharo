public
sortItems: aCollection
	| sorter |
	sorter := self items: aCollection.
	sorter externalRequirements do: [:req  | sorter addProvision: req].
	^ sorter orderedItems.