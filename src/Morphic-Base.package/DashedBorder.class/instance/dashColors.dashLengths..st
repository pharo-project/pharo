accessing
dashColors: cols dashLengths: lens
	"Set the colours and lengths."

	cols size = lens size ifFalse: [self error: 'Colors and Lengths must have the same size'].
	self
		dashColors: cols;
		dashLengths: lens