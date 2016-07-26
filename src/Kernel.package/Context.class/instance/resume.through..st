controlling
resume: value through: firstUnwindContext
	"Unwind thisContext to self and resume with value as result of last send.
	 Execute any unwind blocks while unwinding.
	 ASSUMES self is a sender of thisContext."

	| context unwindBlock |
	self isDead 
		ifTrue: [ self cannotReturn: value to: self ].
	context := firstUnwindContext.
	[ context isNil ] whileFalse: [	
		context unwindComplete ifNil:[
			context unwindComplete: true.
			unwindBlock := context unwindBlock.
			thisContext terminateTo: context.
			unwindBlock value].
		context := context findNextUnwindContextUpTo: self].
	thisContext terminateTo: self.
	^value
