private
withMaximumEntitySizeDo: block
	^ ZnMaximumEntitySize value
		ifNil: [
			ZnMaximumEntitySize 
				value: self maximumEntitySize  
				during: [ ^ block value ] ]
		ifNotNil: [ 
			block value ]