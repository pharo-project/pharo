announcement handling
classRenamed: aClassRenamed

	| aChange |	
	aChange := EpBehaviorNameChange
		oldName: aClassRenamed oldName
		newName: aClassRenamed  newName
		class: aClassRenamed classAffected.
	self addEvent: aChange.