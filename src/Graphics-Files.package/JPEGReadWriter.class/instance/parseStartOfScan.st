marker parsing
parseStartOfScan
	| length n id value dcNum acNum comp |
	length := self nextWord.
	n := self next.
	length ~= (n * 2 + 6) | (n < 1) ifTrue: [ self error: 'SOS length is incorrect' ].
	currentComponents := Array new: n.
	1 
		to: n
		do: 
			[ :i | 
			id := self next.
			value := self next.
			dcNum := value >> 4 bitAnd: 15.
			acNum := value bitAnd: 15.
			comp := components detect: [ :c | c id = id ].
			comp
				dcTableIndex: dcNum + 1;
				acTableIndex: acNum + 1.
			currentComponents 
				at: i
				put: comp ].
	ss := self next.
	se := self next.
	value := self next.
	ah := value >> 4 bitAnd: 15.
	al := value bitAnd: 15.
	self initialSOSSetup.
	self perScanSetup.
	sosSeen := true