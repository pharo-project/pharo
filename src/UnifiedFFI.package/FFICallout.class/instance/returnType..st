spec parsing
returnType: aType
	aType first = 'receiver'
		ifTrue: [ 
			| rcvr rqstr |
			"check that:
				1. method belongs to class side
				2. method class is subclass of NBExternalObject
				3. receiver is the same or subclass of the requestor -- since the requestor is the implementor"
			rcvr := self receiver.
			rqstr := self requestor soleInstance.
			(rcvr isBehavior
				and: [ (rcvr == rqstr or: [ rcvr inheritsFrom: rqstr ]) ])
				ifTrue: [ ^ self typeName: rcvr name pointerArity: aType second ] ].
	self receiver: nil. "if the type is not 'receiver' then we don't need the receiver of the message so set it to nil"
	^ self typeName: aType first pointerArity: aType second