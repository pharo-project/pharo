*System-Object Events
removeActionsSatisfying: aBlock

	self actionMap keysDo:
		[:eachEventSelector |
			self
   				removeActionsSatisfying: aBlock
				forEvent: eachEventSelector
		]