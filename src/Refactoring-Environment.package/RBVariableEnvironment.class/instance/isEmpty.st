testing
isEmpty
	^ self accessorMethods allSatisfy: [ :each | (self perform: each) isEmpty ]