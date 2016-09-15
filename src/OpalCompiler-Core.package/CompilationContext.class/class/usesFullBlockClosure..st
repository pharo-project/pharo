accessing
usesFullBlockClosure: usesFullBlock
	usesFullBlock ifTrue: [ 
		(BytecodeBackend name endsWith: 'SistaV1') ifFalse: [ 
			UIManager default alert: 'Be careful ! Inconsistency detected !
				It seems that the current bytecode encoder does not support FullBlockClosures.
				It is recommended to switch to another bytecode backend (for example SistaV1)
				or disable FullBlockClosure usage.' ] ].
	UsesFullBlockClosure := usesFullBlock