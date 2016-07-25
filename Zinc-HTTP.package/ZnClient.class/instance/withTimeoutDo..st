private
withTimeoutDo: block
	^ ZnConnectionTimeout value
		ifNil: [
			ZnConnectionTimeout 
				value: self timeout 
				during: [ ^ block value ] ]
		ifNotNil: [ 
			block value ]