private
applyAndUpdateSuperclass: classModification
	^ self track: classModification during: [ :old :new |
		installer superclassChangedFrom: old to: new by: classModification ]