notifications
notifyOfChangedSelector: element from: oldCategory to: newCategory
	(self hasSubject and: [(oldCategory ~= newCategory)]) 
		ifTrue: [
			self subject 
				notifyOfRecategorizedSelector: element 
				from: oldCategory 
				to: newCategory ].