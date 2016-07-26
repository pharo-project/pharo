notifications
notifyOfChangedCategoriesFrom: oldCollectionOrNil to: newCollectionOrNil
	(self hasSubject and: [ oldCollectionOrNil ~= newCollectionOrNil ]) 
		ifTrue: [ SystemAnnouncer uniqueInstance classReorganized: self subject ].