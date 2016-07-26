removing
removeLast: n
	" Make the address of the instruction be the address of the first removed instruction. "
	sourceMapByteIndex := sourceMapByteIndex - n.
	^ (currentSequence removeLast: n) collect: [ :node |
		node isTemp ifFalse: [ self error: 'Should only remove temp accesses!' ].
		node name ]