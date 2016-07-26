queries
categoriesMatching: matchString
	"Return all matching categories"
	
	self categories ifNil: [ ^#() ].
	^ self categories select: [:c | matchString match: c]