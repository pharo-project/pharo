accessing
nextIndex
	
	| s |
	s := 0.
	1 to: digits do: 
		[:i | s := (s bitShift: 8) bitOr: stream next].
	^ s