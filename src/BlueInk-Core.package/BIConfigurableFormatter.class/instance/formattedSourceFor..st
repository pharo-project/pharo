private
formattedSourceFor: aNode
	^ lookaheadCode
		at: aNode
		ifAbsentPut: [ 
			self class new
				indent: self indent;
				installNewContext: context;
				format: aNode ]