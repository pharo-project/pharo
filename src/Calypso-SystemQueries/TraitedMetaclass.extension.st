Extension { #name : #TraitedMetaclass }

{ #category : #'*Calypso-SystemQueries' }
TraitedMetaclass >> tagsForAllMethods [
	"I act as #tagsForMethods but I also takes into account methods comming from traits"

	| allProtocols selectors |

	allProtocols := self organization protocols 
		reject: [ :each | each name = Protocol unclassified | each isExtensionProtocol ].

	selectors := self visibleMethods collect: #selector.

	^ allProtocols
		select: [ :protocol | 
			protocol methodSelectors 
				ifEmpty: [ true ] 
				ifNotEmpty: [ :methods | methods anySatisfy: [ :method | selectors includes: method ] ] ]
		thenCollect: #name
]

{ #category : #'*Calypso-SystemQueries' }
TraitedMetaclass >> visibleMethods [

	^ self methods reject: [ :e | e origin = TraitedClass ]
]
