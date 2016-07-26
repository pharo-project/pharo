comparing
= another

	^ (self class = another class
		and: [ self version = another version ])
		and: [ self repository = another repository ]