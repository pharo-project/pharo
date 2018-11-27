accessing
innerBindingOf: aString

	(self definesClassVariable: aString asSymbol)
		ifTrue: [ ^ EPClassVariableBinding new
				name: aString asSymbol
					ownerName: self name
					objectSpace: bootstrapEnvironment;
				yourself ].
		
	self sharedPools
		do:
			[ :pool | (pool innerBindingOf: aString) ifNotNil: [ :binding | ^ binding ] ].

	self superclass ifNotNil:[	
		(self superclass innerBindingOf: aString)
			ifNotNil: [ :binding | ^ binding ]].

	"If it is a temp or an instance variable"
	^ nil