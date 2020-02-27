Extension { #name : #ClassDescription }

{ #category : #'*Calypso-SystemQueries' }
ClassDescription >> tagsForAllMethods [
	"I act as #tagsForMethods but I also takes into account methods comming from traits"

	| allProtocols |
	allProtocols := self organization protocols reject: [ :each | each name = Protocol unclassified | each isExtensionProtocol ].

	^ allProtocols
		select: [ :protocol | protocol methodSelectors ifEmpty: [ true ] ifNotEmpty: [ :methods | methods anySatisfy: [ :method | self selectors includes: method ] ] ]
		thenCollect: #name
]
