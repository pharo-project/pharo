testing
testDecodeSimpleStrings

	self 
		assert: (self decode: 'Z=FCrich') 
		equals: 'Zürich'.
	self 
		assert: (self decode: 'H=E4tten H=FCte ein =DF im Namen, w=E4ren sie m=F6glicherweise keine H=FCte mehr') 
		equals: 'Hätten Hüte ein ß im Namen, wären sie möglicherweise keine Hüte mehr'