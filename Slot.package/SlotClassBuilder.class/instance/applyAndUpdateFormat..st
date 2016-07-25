private
applyAndUpdateFormat: classModification
	^ self track: classModification during: [ :old :new |
		installer formatChangedFrom: old to: new by: classModification ].