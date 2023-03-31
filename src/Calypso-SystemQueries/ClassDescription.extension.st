Extension { #name : #ClassDescription }

{ #category : #'*Calypso-SystemQueries' }
ClassDescription >> tagsForAllMethods [
	"I act as #tagsForMethods but I also takes into account methods comming from traits"

	| allProtocols |
	allProtocols := self organization protocols reject: [ :protocol | protocol isUnclassified | protocol isExtensionProtocol ].

	^ allProtocols
		select: [ :protocol | protocol methodSelectors ifEmpty: [ true ] ifNotEmpty: [ :methods | methods anySatisfy: [ :method | self selectors includes: method ] ] ]
		thenCollect: #name
]
