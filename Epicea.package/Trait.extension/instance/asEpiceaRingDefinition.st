*Epicea-Ring
asEpiceaRingDefinition

	^ self asRingDefinition
		package: (RPackage organizer packageOfClassNamed: self name) name;  "FIX: see same selector in Class"
		yourself.