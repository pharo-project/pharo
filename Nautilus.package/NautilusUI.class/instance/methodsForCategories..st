widget method
methodsForCategories: aCollection

	^ aCollection flatCollect: [:e | self methodsForCategory: e ]