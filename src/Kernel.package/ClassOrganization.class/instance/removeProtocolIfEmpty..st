accessing
removeProtocolIfEmpty: protocolName
	"The protocol may already have been removed, be non empty or a special protocol which can't be removed, such as 'all'."

	(self protocolNamed: protocolName)
		ifNotNil: [ :protocol | 
			(protocol isEmpty and: [ protocol canBeRemoved ])
				ifTrue: [ self removeProtocol: protocol ] ]