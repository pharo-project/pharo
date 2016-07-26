private
currentTriggerReference

	Job current 	
		ifNil: [ ^ log nullReference ]
		ifNotNil: [ :currentJob |
			currentJob 
				lookup: [ :job | 
					entryReferenceByEvent at: job 
						ifPresent: [ :reference | ^ reference ] 
						ifAbsent: [ false ] ]
				ifNone: [ ^ log nullReference ].
			].