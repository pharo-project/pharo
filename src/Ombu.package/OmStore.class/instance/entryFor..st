accessing
entryFor: aReference
	"Look for an entry"

	^ self
		entryFor: aReference
		ifPresent: [ :anEntry | ^ anEntry ] 
		ifAbsent: [ NotFound signalFor: aReference ]
