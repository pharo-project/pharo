stack list
updateStackList: depth 
	| suspendedContext oldHighlight |
	selectedProcess
		ifNil: [^ self changeStackListTo: nil].
	(stackList notNil and: [ stackListIndex > 0 ])
		ifTrue: [oldHighlight := stackList at: stackListIndex].
	selectedProcess == Processor activeProcess
		ifTrue: [self
				changeStackListTo: (thisContext stackOfSize: depth)]
		ifFalse: [suspendedContext := selectedProcess suspendedContext.
			suspendedContext
				ifNil: [self changeStackListTo: nil]
				ifNotNil: [self
						changeStackListTo: (suspendedContext stackOfSize: depth)]].
	self
		stackListIndex: (stackList
				ifNil: [0]
				ifNotNil: [stackList indexOf: oldHighlight])