instruction decoding
jump: offset
	| loop backjumpTarget |
	offset < 0 ifFalse: [ ^ self ].
	loop := (branchTargets at: self pc)
		ifNotNil: [ :exitPC | FBDConditionalLoop new exitCondition: exitPC; yourself ]
		ifNil: [ FBDLoop new ].
	backjumpTarget := self pc + offset.
	loop backjump: currentPC.
	(loops at: backjumpTarget) 
		ifNil: [ loops at: backjumpTarget put: (OrderedCollection with: loop) ]
		ifNotNil: [ :col | col add: loop ].