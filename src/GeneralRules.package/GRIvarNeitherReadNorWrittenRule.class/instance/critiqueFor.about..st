enumerating
critiqueFor: aClass about: aSlot
	| crit |
	crit := ReTrivialCritique
		withAnchor: (ReVarSearchSourceAnchor
			entity: aClass
			string: aSlot name)
		by: self.
	
	crit tinyHint: aSlot name.
	^ crit