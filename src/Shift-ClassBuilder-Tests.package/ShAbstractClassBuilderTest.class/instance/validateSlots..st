validation
validateSlots: allSlots
	| resultSlots |
	resultSlots := result allSlots.

	self
		assert: resultSlots size = allSlots size
		description: (self comparingCollectionBetween: resultSlots and: allSlots).

	allSlots
		with: resultSlots
		do: [ :a :b | 
			self assert: a name equals: b name.
			self assert: a class equals: b class ]