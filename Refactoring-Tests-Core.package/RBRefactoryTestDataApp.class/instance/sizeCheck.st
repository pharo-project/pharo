lint
sizeCheck
	self isEmpty
		ifFalse: [self do: 
					[:each | 
					Transcript
						show: each;
						cr]]