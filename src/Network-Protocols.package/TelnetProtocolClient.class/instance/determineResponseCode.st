private
determineResponseCode
	self lastResponse size >= 3
		ifFalse: [^0].
	^[SmallInteger readFromString: (self lastResponse copyFrom: 1 to: 3)]
		on: Error
		do: [:ex | ex return: 0]