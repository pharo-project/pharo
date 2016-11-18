protocol - removing
removeEmptyProtocols
	| removedProtocols |
	removedProtocols := protocols select: [:e | e isEmpty and: [ e canBeRemoved ] ].
	removedProtocols do: [:each | self removeProtocol: each].
	^removedProtocols