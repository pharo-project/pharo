visitor
= anObject

	^ (self class = anObject class)
		and: [ self versionNames = anObject versionNames ]