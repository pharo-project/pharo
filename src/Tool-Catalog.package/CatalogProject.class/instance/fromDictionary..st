initialization
fromDictionary: aDictionary 
	name := aDictionary at: #name.
	packageName := aDictionary at: #packageName.
	repositoryUrl := aDictionary at: #repositoryUrl.
	description := aDictionary at: #description ifAbsent: [ nil ].
	contactInfo := aDictionary at: #contactInfo ifAbsent: [ nil ].
	keywords := ((aDictionary 
		at: #keywords  
		ifAbsent: [ #() ])
		ifNil: [ #() ])
		collect: #asLowercase.