accessing
entryFor: aReference
	"Answer the entry corresponding to aReference"

	^ self
		entryFor: aReference
		ifPresent: [ :anEntry | ^ anEntry ] 
		ifAbsent: [ NotFound signalFor: aReference ]
