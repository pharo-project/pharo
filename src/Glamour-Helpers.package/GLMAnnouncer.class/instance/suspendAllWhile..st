suspending
suspendAllWhile: aBlock
	| previousSuspensionState |
	previousSuspensionState := suspendAll.
	suspendAll := true.
	aBlock ensure: [suspendAll := previousSuspensionState]
	