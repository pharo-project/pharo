printing
destinationBuffer:digitLength
  digitLength <= 1
		ifTrue: [self]
		ifFalse: [LargePositiveInteger new: digitLength].