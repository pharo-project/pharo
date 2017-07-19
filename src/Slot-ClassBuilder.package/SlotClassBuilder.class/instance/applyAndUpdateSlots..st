private
applyAndUpdateSlots: classModification
	^ self track: classModification during: [ :old :new |
		installer slotsChangedFrom: old to: new by: classModification ].