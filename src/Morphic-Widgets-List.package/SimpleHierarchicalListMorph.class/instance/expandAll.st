actions
expandAll
	"Expand all of the roots!"
	
	self roots do: [:m |
		self expandAll: m].
	self adjustSubmorphPositions