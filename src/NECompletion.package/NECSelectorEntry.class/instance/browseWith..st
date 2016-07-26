private
browseWith: anECContext

	(self guessTypeWith: anECContext)
		ifNotNil: [ :clazz|
			Smalltalk tools browser openOnClass: clazz.
			^ true ].
	
	^ self
		findMethodWith: anECContext
		do: [ :class :method | 
			Smalltalk tools browser 
				openOnClass: class 
				selector: method selector. 
			true ]
		ifAbsent: [ :selector | 
			SystemNavigation new 
				browseAllImplementorsOf: selector.
			true ]