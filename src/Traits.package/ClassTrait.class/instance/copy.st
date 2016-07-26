copying
copy
	"Make a copy of the receiver. Share the 
	reference to the base trait."

	^(self class new)
		baseTrait: self baseTrait;
		initializeFrom: self;
		yourself