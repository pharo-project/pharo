private
applyAndUpdateSharedVariableOrSharedPool: classModification
	^ self track: classModification during: [ :old :new |
		installer sharedVariableOrSharedPoolChangedFrom: old to:  new by: classModification ].