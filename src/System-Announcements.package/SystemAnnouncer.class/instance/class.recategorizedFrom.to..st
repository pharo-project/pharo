triggering
class: aClass recategorizedFrom: oldCategory to: newCategory 
	self announce: (ClassRecategorized 
						class: aClass
						recategorizedFrom: oldCategory
						to: newCategory)