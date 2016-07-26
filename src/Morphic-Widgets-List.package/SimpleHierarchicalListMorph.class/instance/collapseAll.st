accessing
collapseAll
	"Expand all of the roots!"
	
	self roots do: [:m |
		self collapseAll: m].
	self adjustSubmorphPositions