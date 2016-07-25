tests
testSlotNamedIfFoundIfNone

	aClass := self make: [ :builder |
		builder 
			name: self aClassName;
			slots: #(a) 
		].

	self 
		assert: (aClass slotNamed: 'a' ifFound: [ #found ] ifNone: [ #none ])
		equals: #found.

	self 
		assert: (aClass slotNamed: 'b' ifFound: [ #found ] ifNone: [ #none ])
		equals: #none.
