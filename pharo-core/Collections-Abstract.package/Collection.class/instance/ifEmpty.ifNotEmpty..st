testing
ifEmpty: emptyBlock ifNotEmpty: notEmptyBlock
	"Evaluate emptyBlock if I'm empty, notEmptyBlock otherwise"
	" If the notEmptyBlock has an argument, eval with the receiver as its argument"

	self isEmpty ifTrue: [ ^emptyBlock value ].
	^notEmptyBlock cull: self