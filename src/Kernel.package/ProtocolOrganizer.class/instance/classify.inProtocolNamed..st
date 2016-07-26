protocol - adding
classify: aSymbol inProtocolNamed: aProtocolName 
	| name protocol |
	
	name := aProtocolName.
	name = allProtocol name
		ifTrue: [ name := Protocol unclassified ].
		
	"maybe here we should check if this method already belong to another protocol"
	(self protocolsOfSelector: aSymbol) do: [:p | p removeMethod: aSymbol ].
	protocol := self getProtocolNamed: name ifNone: [ self addProtocolNamed: name ].
	
	protocol addMethod: aSymbol
	