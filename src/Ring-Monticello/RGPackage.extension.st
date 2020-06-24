Extension { #name : #RGPackage }

{ #category : #'*Ring-Monticello' }
RGPackage >> asMCSnapshot [

	| definitions | 
	
	definitions := ((((self definedBehaviors reject: [:each | each isMeta]) sorted: [:a :b | a name < b name ]) collect: [:each | each asMCDefinition]), (self extensionMethods collect: [:each | each asMCDefinition])) asOrderedCollection.
	definitions addAll: ((self definedBehaviors, (self definedBehaviors collect: [:each | each classSide])) flatCollect: [ :behavior |
		(behavior localMethods reject: [:each | (each  parent instanceSide package = each package) not])
			collect: [ :method | method asMCDefinition] ]).
	
	^ MCSnapshot fromDefinitions: definitions.
]
