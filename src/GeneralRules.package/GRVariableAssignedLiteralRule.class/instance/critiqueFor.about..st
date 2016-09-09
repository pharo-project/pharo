running-helpers
critiqueFor: aClass about: aVarName
	| crit |

	crit := ReTrivialCritique
		withAnchor: (ReVarSearchSourceAnchor
			entity: aClass
			string: aVarName)
		by: self.
	
	crit tinyHint: aVarName.
				
	^ crit