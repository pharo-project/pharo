menu - change set
checkThatSidesDiffer: escapeBlock
	"If the change sets on both sides of the dual sorter are the same, put up an error message and escape via escapeBlock, else proceed happily"

	changeSorterLeft selectedChangeSet == changeSorterRight selectedChangeSet
		ifFalse: [ ^ self ].
	self
		inform:
			'This command requires that the
change sets selected on the two
sides of the change sorter *not*
be the same.'.
	^ escapeBlock value