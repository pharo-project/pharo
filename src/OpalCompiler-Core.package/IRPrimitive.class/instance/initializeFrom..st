accessing
initializeFrom: aPragmaNode

	primitiveNum := 0.
	spec := Array 
		with: (aPragmaNode arguments second) name
		with: (aPragmaNode arguments first) name
		with: 0 with: 0.
