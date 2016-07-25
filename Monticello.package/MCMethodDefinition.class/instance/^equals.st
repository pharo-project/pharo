comparing
= aDefinition
	^ super = aDefinition
		and: [ 
			aDefinition category = self category
				and: [ aDefinition source withSqueakLineEndings = self source withSqueakLineEndings ] ]