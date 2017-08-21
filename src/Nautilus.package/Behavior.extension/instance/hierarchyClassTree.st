*Nautilus
hierarchyClassTree

^{
	self . 

	( self subclasses 
	reject: [ :class | class isClassSide ] 
	thenCollect: [ :cls | cls hierarchyClassTree ]
	) sort: [ :a :b | a first name <= b first name ]
} 