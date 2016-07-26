printing
printOn: aStream 

	self 
		print24: false
		showSeconds: (self seconds ~= 0 or: [ self nanoSecond ~= 0 ])
		on: aStream