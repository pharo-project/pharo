notifications
notifyOfChangedCategoryFrom: oldNameOrNil to: newNameOrNil
	(self hasSubject and: [oldNameOrNil ~= newNameOrNil]) 
		ifTrue: [SystemAnnouncer uniqueInstance classReorganized: self subject].