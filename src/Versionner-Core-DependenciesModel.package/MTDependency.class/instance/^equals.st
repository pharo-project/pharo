comparing
= other
	(self class = other class) ifFalse: [ ^ false ].
	(self name = other name) ifFalse: [ ^ false ].
	(self version = other version) ifFalse: [ ^ false ].
	(self parent = other parent) ifFalse: [ ^ false ].
	"(self platforms  = other platforms) ifFalse: [ ^false ]. "
	^ true