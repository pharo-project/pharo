private
formattedSourceFor: aNode
	^ lookaheadCode
		at: aNode
		ifAbsentPut: [ 
			self class new
				indent: 1;
				format: aNode ]