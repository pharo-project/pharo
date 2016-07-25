private
sortCategories: protocols forClass: class
	^ self 
		sortCategories: protocols
		forClass: class
		firstInPackage: self model selectedPackage