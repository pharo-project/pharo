pool variables
sharedPools
	"Answer an orderedCollection  of the shared pools declared in the receiver."

	^ sharedPools ifNil: [ sharedPools := OrderedCollection new ]