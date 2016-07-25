private
withProgressDo: block
	^ ZnSignalProgress value
		ifNil: [
			ZnSignalProgress 
				value: self signalProgress 
				during: [ ^ block value ] ]
		ifNotNil: [ 
			block value ]