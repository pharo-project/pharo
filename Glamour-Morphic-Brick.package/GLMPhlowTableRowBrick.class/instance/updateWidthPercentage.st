updating
updateWidthPercentage
	
	self subbricks ifEmpty: [ ^ self ].
	self subbricks do: [ :each | each hSpaceFill: (100 / self subbricksCount) ]