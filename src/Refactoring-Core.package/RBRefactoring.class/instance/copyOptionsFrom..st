accessing
copyOptionsFrom: aDictionary 
	| dict |
	dict := self options.
	dict == self class refactoringOptions 
		ifTrue: [^self options: aDictionary copy].
	dict keysAndValuesDo: 
			[:key :value | 
			value == (self class refactoringOptions at: key) 
				ifTrue: [dict at: key put: (aDictionary at: key)]].
	self options: dict