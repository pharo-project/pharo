instructions
pushInstVar: instVarIndex
	stack push.
	forceLongForm
		ifTrue: [ encoder genPushInstVarLong: instVarIndex - 1 ]
		ifFalse: [ encoder genPushInstVar: instVarIndex - 1 ]