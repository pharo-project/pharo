Extension { #name : 'RGPackage' }

{ #category : '*Ring-Monticello' }
RGPackage >> asMCSnapshot [

	| definitions |
	definitions := OrderedCollection new.
	definitions addAll: ((self definedBehaviors reject: [ :each | each isMeta ]) collect: [ :each | each asMCDefinition ]).
	definitions addAll: (self extensionMethods collect: [ :each | each asMCDefinition ]).
	definitions addAll: (self definedBehaviors , (self definedBehaviors collect: [ :each | each classSide ]) flatCollect: [ :behavior |  (behavior localMethods select: [ :each | each parent instanceSide package = each package ]) collect: [ :method | method asMCDefinition ] ]).
	definitions add: (MCOrganizationDefinition packageName: name).
	^ MCSnapshot fromDefinitions: definitions
]
