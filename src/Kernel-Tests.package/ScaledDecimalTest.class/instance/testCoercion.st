tests
testCoercion
	#( #* #+ #- #/) do: [:op |
		self assert: (1.0s1 perform: op with: 2) class = ScaledDecimal.
		self assert: (1.0s1 perform: op with: 1/2) class = ScaledDecimal.
		self deny: (1.0s1 perform: op with: 1.0) class = ScaledDecimal.
		
		self assert: (1 perform: op with: 2.0s1) class = ScaledDecimal.
		self assert: (1/2 perform: op with: 2.0s1) class = ScaledDecimal.
		self deny: (1.0 perform: op with: 1.0s1) class = ScaledDecimal]