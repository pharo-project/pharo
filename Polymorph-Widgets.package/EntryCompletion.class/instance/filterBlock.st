accessing
filterBlock

	^ filterBlock ifNil: [filterBlock := [:currApplicant  :currText | true]]