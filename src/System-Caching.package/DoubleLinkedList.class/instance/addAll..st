adding
addAll: collection
	"Add all elements of collection to me, in order.
	Each can be either a value object or a link."
	
	collection do: [ :each | self add: each ]