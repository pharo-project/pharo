copying
copyWithout: oldElement
	|newInst| 
	newInst := self class new.
	self do: [:each | each = oldElement ifFalse: [newInst add: each]].
	^newInst