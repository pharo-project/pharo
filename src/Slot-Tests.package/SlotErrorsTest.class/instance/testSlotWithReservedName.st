tests
testSlotWithReservedName

	#(#self #super #thisContext #true #false #nil) do: [:reservedName |
		self should: [ 
			self make: [ :builder | 
				builder slots: { reservedName } ] ]
			raise: InvalidSlotName ]