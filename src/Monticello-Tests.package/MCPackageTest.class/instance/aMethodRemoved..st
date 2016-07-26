private
aMethodRemoved: anEvent
	"Force cleaning of the protocol."

	(anEvent protocol = self mockExtensionMethodCategory and: [ anEvent methodClass == MCSnapshotTest ])
		ifTrue: [ anEvent methodClass organization removeProtocolIfEmpty: self mockExtensionMethodCategory ]