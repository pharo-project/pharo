enumerating
from: aReference detect: aBlockReturningBoolean ifNotFound: notFoundBlock

	self
		priorEntriesFrom: aReference
		do: [ :entry |
			(aBlockReturningBoolean value: entry)
				ifTrue: [ ^ entry ] ].
	^ notFoundBlock cull: aReference