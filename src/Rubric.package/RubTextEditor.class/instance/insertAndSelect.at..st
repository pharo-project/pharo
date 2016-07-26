new selection
insertAndSelect: aString at: anInteger

	self replace: (anInteger to: anInteger - 1)
		with: (Text string: (' ' , aString)
					attributes: self emphasisHere)
		and: [self ]