registrations handling
pragmaCollector
	"Return an up-to-date pragmaCollector which contains all pragmas which keyword is self pragmaKeyword"

	^ pragmaCollector
		ifNil: [ pragmaCollector := PragmaCollector
				filter:
					[ :prg | (self pragmaKeywords includes: prg selector) and: [ prg methodSelector numArgs = 1 ] ].
			(self pragmaKeywords isNotNil and: [ self pragmaKeywords isNotEmpty ])
				ifTrue: [ pragmaCollector reset ].
			pragmaCollector whenChangedSend: #reset to: self.
			pragmaCollector ]