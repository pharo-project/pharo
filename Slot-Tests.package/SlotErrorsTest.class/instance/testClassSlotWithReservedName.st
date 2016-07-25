tests
testClassSlotWithReservedName

	#(#self #super #thisContext #true #false #nil) do: [:reservedName |
		self should: [ 
			self make: [ :builder | 
				builder classSlots: { reservedName } ] ]
			raise: InvalidSlotName ]