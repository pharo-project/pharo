crc
verifyCrc
	| stored |
	stored := 0.
	24 to: 0 by: -8 do: [ :i |
		sourcePos >= sourceLimit ifTrue: [ ^ self crcError: 'No checksum (proceed to ignore)' ].
		stored := stored + (self nextByte bitShift: i) ].
	stored = crc ifFalse: [ ^ self crcError: 'Wrong checksum (proceed to ignore)' ].
	^stored