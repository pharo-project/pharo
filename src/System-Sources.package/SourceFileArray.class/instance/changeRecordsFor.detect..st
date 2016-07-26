public - string reading
changeRecordsFor: aMethodDefinition detect: aUnaryBlock
	"Try to detect the most recent ChangeRecord that satisfies aUnaryBlock. Answer nil if none satisfies."

	self
		changeRecordsFor: aMethodDefinition
		do: [ :changeRecord | 
			(aUnaryBlock value: changeRecord category) 
				ifTrue: [ ^ changeRecord ] ].
	^ nil