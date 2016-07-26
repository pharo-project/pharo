comparing
= aRGClassDefinition

	^self class = aRGClassDefinition class 
		and: [ self name = aRGClassDefinition name 
			and: [ self environment = aRGClassDefinition environment ] ]