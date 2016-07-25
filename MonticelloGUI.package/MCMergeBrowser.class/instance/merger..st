actions
merger: aMerger
	merger := aMerger.
	items := aMerger operations asSortedCollection.
	conflicts := aMerger conflicts sort: [:a :b | a operation <= b operation].