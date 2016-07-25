column handling
columnsChanged
	"A column has been added or removed or swapped with another one
	- rebuild all resizers and the top header"
	self removeColumnResizers.
	self removeTopHeader.
	self buildTopHeader.
	self addColumnResizers.
