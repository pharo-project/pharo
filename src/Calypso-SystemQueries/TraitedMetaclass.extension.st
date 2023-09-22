Extension { #name : 'TraitedMetaclass' }

{ #category : '*Calypso-SystemQueries' }
TraitedMetaclass >> protocolsForAllMethods [

	| allProtocols selectors |
	allProtocols := self protocols reject: [ :protocol | protocol isUnclassifiedProtocol or: [ protocol isExtensionProtocol ] ].

	selectors := self visibleMethods collect: [ :method | method selector ].

	^ allProtocols
		  select: [ :protocol | protocol methodSelectors ifEmpty: [ true ] ifNotEmpty: [ :methods | methods anySatisfy: [ :method | selectors includes: method ] ] ]
		  thenCollect: #name
]

{ #category : '*Calypso-SystemQueries' }
TraitedMetaclass >> visibleMethods [

	^ self methods reject: [ :e | e origin = TraitedClass ]
]
