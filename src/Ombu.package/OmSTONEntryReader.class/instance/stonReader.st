accessing
stonReader

	^ reader ifNil: [ 
		reader := STON reader
			allowComplexMapKeys: true;
			yourself ]