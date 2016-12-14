accessing
bindingOf: aString
	(self definesClassVariable: aString asSymbol)
		ifTrue: [ ^ EPClassVariableBinding new
				name: aString asSymbol
					ownerName: self name
					objectSpace: bootstrapEnvironment;
				yourself ].
	self sharedPools
		do:
			[ :pool | (pool bindingOf: aString) ifNotNil: [ :binding | ^ binding ] ].
	(self superclass bindingOf: aString)
		ifNotNil: [ :binding | ^ binding ].
	(bootstrapEnvironment includesClassNamed: aString)
		ifTrue: [ ^ EPGlobalBinding new
				name: aString objectSpace: bootstrapEnvironment;
				yourself ].

	"If it is a temp or an instance variable"
	^ nil