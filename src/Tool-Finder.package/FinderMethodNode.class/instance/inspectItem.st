private
inspectItem
	self hasParentNode ifTrue: [ (self parentNode item compiledMethodAt: self item) inspect ].