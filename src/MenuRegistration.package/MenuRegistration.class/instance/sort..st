accessing
sort: aSortBlock
	"Sort the tree recursively"
	self itemList
		ifNotNil: [
		self itemList: (self itemList asSortedCollection: aSortBlock).
		self itemList do: [:rded | rded sort: aSortBlock] ]