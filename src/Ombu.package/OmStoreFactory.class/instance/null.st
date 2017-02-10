accessing
null

	^ nullStore ifNil: [ nullStore := OmNullStore new ]