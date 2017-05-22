private
basicSort: sortingEnum
	sorting := sorting
		ifNil: [ sortingEnum value ]
		ifNotNil: [ sorting bitOr: sortingEnum value ].
		
	self withReturnHandlerDo: [
		self
			revwalk_sorting: self
			sort_mode: sorting ]