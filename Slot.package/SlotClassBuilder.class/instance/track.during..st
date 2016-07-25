private
track: classModification during: aBlock
	| old new |
	old := classModification target.
	new := classModification apply.

	aBlock value: old value: new.
	
	" Store the modification for updateClasses "
	oldClasses add: old.
	newClasses add: new.
	^ new