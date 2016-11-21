notifications
notifyOfChangedCategoryFrom: oldNameOrNil to: newNameOrNil
	(self hasSubject and: [oldNameOrNil ~= newNameOrNil]) 
		ifTrue: [
			SystemAnnouncer uniqueInstance 
				protocolRenamedFrom: oldNameOrNil 
				to: newNameOrNil 
				inClass: self subject.
			SystemAnnouncer uniqueInstance classReorganized: self subject].