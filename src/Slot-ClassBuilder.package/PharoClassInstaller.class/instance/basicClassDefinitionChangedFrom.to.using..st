notifications
basicClassDefinitionChangedFrom: oldClass to: newClass using: classModification

	" Copy over the trait composition "
	self copyTraitCompositionFrom: oldClass to: newClass.		
	" Copy over the method organization "
	newClass organization: oldClass organization.
	" Update the subclass links "
	oldClass superclass == newClass superclass ifFalse: [ 
		oldClass superclass removeSubclass: oldClass.
		newClass superclass addSubclass: newClass ].
	" Announce if necessary "
	classModification isPropagation ifFalse: [ 
		self systemAnnouncer classDefinitionChangedFrom: oldClass to: newClass ].