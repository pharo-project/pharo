testing
isGlobalClassNameBinding
	^ (self value isClass or: [ self value isTrait ])
		and: [ self variable key == self value name ]