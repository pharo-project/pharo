crc
verifyCrc
	| stored |
	stored := 0.
	0 to: 24 by: 8 do: [ :i |
		sourcePos >= sourceLimit ifTrue: [ ^ self crcError: 'No checksum (proceed to ignore)' ].
		stored := stored + (self nextByte bitShift: i) ].
	stored := stored bitXor: 16rFFFFFFFF.
	stored = crc ifFalse: [ ^ self crcError: 'Wrong checksum (proceed to ignore)' ].
	^stored