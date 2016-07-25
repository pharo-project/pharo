system compatibility
changeRecordForOverriddenMethod: aMethodDefinition
	^ SourceFiles
		changeRecordsFor: aMethodDefinition 
		detect: [ :protocol | 
			self 
				includesMethodCategory: protocol 
				ofClass: aMethodDefinition actualClass ]