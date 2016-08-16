helpers
critiqueFor: aPackage

	^ ArchitecturalCritique
		withAnchor: (self anchorFor: aPackage)
		by: self