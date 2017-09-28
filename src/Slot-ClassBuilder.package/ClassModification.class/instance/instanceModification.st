accessing
instanceModification
	| map |
	map := InstanceModification new: layout fieldSize.
	^ self buildModificationMapFor: map