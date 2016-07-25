converting
addAttributesFrom: attributeRuns satisfying: aTestBlock to: aText

	attributeRuns withStartStopAndValueDo: [:start :stop :attributes | 
		attributes 
			select: aTestBlock 
			thenDo: [ :selectedAttribute| 
				aText
					addAttribute: selectedAttribute
					from: start
					to: stop ]].
		
	^ aText