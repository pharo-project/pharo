running
execute: aWorkingCopy
	aWorkingCopy packageSet methods do: [ :each |
		(each classIsMeta and: [ each selector = #initialize ])
			ifTrue: [ each actualClass theNonMetaClass initialize ] ]