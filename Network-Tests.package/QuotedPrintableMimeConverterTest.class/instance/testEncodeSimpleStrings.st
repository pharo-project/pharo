testing
testEncodeSimpleStrings

	self 
		assert: (self encode: 'Zürich')
		equals: 'Z=FCrich'.
	self 
		assert: (self encode: 'Über die Fährverbindungen nach Föhr wüßte ich nicht viel')
		equals: '=DCber die F=E4hrverbindungen nach F=F6hr w=FC=DFte ich nicht viel'