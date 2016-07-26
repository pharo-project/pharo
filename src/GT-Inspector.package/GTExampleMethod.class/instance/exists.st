testing
exists
	^ self provider notNil
		and: [ self providerClass notNil
			and: [ self providerClass includesSelector: self selector ] ]