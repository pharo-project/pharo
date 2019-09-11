Extension { #name : #RGPackage }

{ #category : #'*Ring-Monticello' }
RGPackage >> asMCSnapshot [

	| definitions | 
	
	definitions := ((((self definedBehaviors reject: #isMeta) sorted: [:a :b | a name < b name ]) collect: #asMCDefinition), (self extensionMethods collect: #asMCDefinition)) asOrderedCollection.
	definitions addAll: ((self definedBehaviors, (self definedBehaviors collect: #classSide)) flatCollect: [ :behavior |
		(behavior localMethods reject: [:each | (each  parent instanceSide package = each package) not])
			collect: [ :method | method asMCDefinition] ]).
	
	^ MCSnapshot fromDefinitions: definitions.
]
