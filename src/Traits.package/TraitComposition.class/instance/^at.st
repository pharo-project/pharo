composition
@ anArrayOfAssociations
	"the modifier operators #@ and #- bind stronger than +"

	self transformations: (self transformations collect: [ :t | t @ anArrayOfAssociations ]).