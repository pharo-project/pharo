*Versionner-Commit
isDirty
	^ self workingCopy 
		ifNotNil: [ :wc | wc modified ]
		ifNil: [ false ]