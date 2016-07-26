testing
ifNotEmpty: notEmptyBlock ifEmpty: emptyBlock
	"Evaluate emptyBlock if I'm empty, notEmptyBlock otherwise
	 If the notEmptyBlock has an argument, eval with the receiver as its argument"

	self isEmpty ifFalse: [ ^notEmptyBlock cull: self ].
	^emptyBlock value