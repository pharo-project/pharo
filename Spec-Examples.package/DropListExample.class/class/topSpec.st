specs
topSpec

	^ SpecLayout composed
		newRow: [ :r |
			r 
				add: #uniformDropList;
				add: #heterogeneousDropList ];
		yourself