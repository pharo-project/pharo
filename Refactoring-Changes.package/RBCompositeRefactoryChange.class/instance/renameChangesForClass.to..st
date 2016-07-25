accessing
renameChangesForClass: oldClassName to: newClassName
	^ self copy
		changes: (self changes 
			collect: [ :each | each renameChangesForClass: oldClassName to: newClassName ]);
		yourself