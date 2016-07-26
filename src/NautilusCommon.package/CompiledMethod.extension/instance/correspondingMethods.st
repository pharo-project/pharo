*NautilusCommon
correspondingMethods
	" retrieve methods that could be the corresponding method "

	self isTestMethod
		ifTrue: [ 
			self methodClass isTestCase
				ifTrue: [ 
					| classSymbol correspondingClass correspondingSel className selector |
					selector := self selector.
					className := self methodClass name.
					(selector beginsWith: 'test') ifFalse: [ ^ #() ].
					classSymbol := (className endsWith: 'Test')
						ifTrue: [ className allButLast: 4]
						ifFalse: [ className ]. 
					correspondingClass := self class environment at: classSymbol asSymbol ifAbsent: [ ^ #() ].
					correspondingSel := (selector allButFirst: 4) transformSelectorForComparison.
					^ correspondingClass selectors
						select: [ :each | each asLowercase = correspondingSel asLowercase or: [ each asLowercase = (correspondingSel , ':') asLowercase ] ]
						thenCollect: [ :each | correspondingClass >> each ] ]
				ifFalse: [ ^ #() ] ]
		ifFalse: [ ^ {self} ]