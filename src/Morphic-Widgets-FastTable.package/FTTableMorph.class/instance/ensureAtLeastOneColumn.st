private
ensureAtLeastOneColumn
	self columns ifNotEmpty: [ ^ self ].
	self addColumn: self class defaultColumn