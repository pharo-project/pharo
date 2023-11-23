Extension { #name : 'ClassDescription' }

{ #category : '*Calypso-SystemQueries' }
ClassDescription >> protocolsForAllMethods [

	^ self protocols
		  reject: [ :protocol | protocol isUnclassifiedProtocol or: [ protocol isExtensionProtocol ] ]
		  thenCollect: #name
]
