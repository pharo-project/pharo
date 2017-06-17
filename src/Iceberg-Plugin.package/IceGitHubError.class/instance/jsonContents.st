accessing
jsonContents
	^ jsonContents ifNil: [ jsonContents := STON fromString: self contents ]	
	