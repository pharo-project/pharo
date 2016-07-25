detail information
createDescriptionWith: anECContext

	(self guessTypeWith: anECContext)
		ifNotNil: [ :clazz|
			^ NECEntryDescription 
					label: self label
					title: clazz printString
					description: clazz comment	].
	^ self
		findMethodWith: anECContext
		do: [:clazz :method | 
			self
				methodSourceDescription: clazz
				method: method ]
		ifAbsent: [:selector | self implementorsDescription: selector]