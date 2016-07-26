unloading
unloadClasses: aWorkingCopy
	aWorkingCopy packageSet methods do: [ :each |
		(each classIsMeta and: [ each selector = #unload ])
			ifTrue: [ each actualClass theNonMetaClass unload ] ]