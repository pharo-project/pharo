updating
filterWith: aStringOrText
	initialDataSource ifNil: [ self initializeFilter ].	"I do this in case the filter is use explicictly in the FT, at the first call the Filter will not be initialize."
	pattern := aStringOrText asString trimBoth.
	isEditingSemaphore signal