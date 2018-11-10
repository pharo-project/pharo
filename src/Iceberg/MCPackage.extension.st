Extension { #name : #MCPackage }

{ #category : #'*Iceberg' }
MCPackage >> basicSnapshot [
	| rPackageSet definitions categories |
	rPackageSet := self packageSet.
	definitions := OrderedCollection new.
	categories := rPackageSet categoryNames asArray.
	categories isEmpty 
		ifFalse: [ definitions add: (MCOrganizationDefinition categories: categories) ].
		
	rPackageSet methods 
		do: [:ea | definitions add: ea asMCMethodDefinition].
		
	rPackageSet definedClasses 
		do: [:ea | definitions addAll: ea classDefinitions].
		
	^ MCSnapshot fromDefinitions: definitions

]
