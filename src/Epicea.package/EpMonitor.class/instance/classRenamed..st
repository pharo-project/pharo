announcement handling
classRenamed: aClassRenamed

	self handleAnyErrorDuring: [
		self addEvent: (EpBehaviorNameChange
			oldName: aClassRenamed oldName
			newName: aClassRenamed  newName
			class: aClassRenamed classAffected) ]