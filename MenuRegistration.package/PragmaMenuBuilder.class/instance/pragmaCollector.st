registrations handling
pragmaCollector
	"Return an up-to-date pragmaCollector which contains all pragmas which keyword is self pragmaKeyword"

	^ pragmaCollector
		ifNil: [ pragmaCollector := PragmaCollector
				filter: [ :prg | 
					(self pragmaKeywords includes: prg keyword)
						and: [ prg methodSelector numArgs = 1 ] ].
			(self pragmaKeywords notNil and: [ self pragmaKeywords notEmpty ])
				ifTrue: [ pragmaCollector reset ].
			pragmaCollector whenChangedSend: #reset to: self.
			pragmaCollector ]