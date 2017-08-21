queries
copiedMethodsFromSuperclass
	"Returns the methods that the receiver copied with its ancestors"
	
	| methods |
	methods := OrderedCollection new.
	self methodsDo: [ :method|
		methods addAll:  (self copiedFromSuperclass: method)].
	^ methods