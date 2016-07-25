*System-Object Events
removeActionsWithReceiver: anObject

	self actionMap copy keysDo:
		[:eachEventSelector |
			self
   				removeActionsSatisfying: [:anAction | anAction receiver == anObject]
				forEvent: eachEventSelector
		]