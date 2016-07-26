initialization
initialize
	silent := false.
	super initialize.
	self hasContentToShow: true.
	self whenSelectedChanged: [ :aBoolean | self content selected: aBoolean ].
	self whenIsExpandedChanged: [ :aBoolean | self content expanded: aBoolean ].
	self
		whenContentChanged:
			[ :c | 
			self silentWhile: [ self selected: c selected ].
			self isExpanded: c expanded ].
	self hasChildren: [ self children value isEmptyOrNil not ]