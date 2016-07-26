*renraku
overriddenMethods
	| overridenMethods |
	
	overridenMethods := OrderedCollection new.

	self methodClass allSuperclasses
		do: [ :class |
			class compiledMethodAt: self selector
				ifPresent: [ :method | overridenMethods add: method ]
				ifAbsent: [ ] ].
			
	^ overridenMethods 