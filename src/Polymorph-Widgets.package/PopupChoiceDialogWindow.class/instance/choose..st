as yet unclassified
choose: index
	"Set the given choice and ok."

	self choice: (self model
		ifNil: [index]
		ifNotNil: [self model at: index]).
	self ok