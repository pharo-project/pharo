protocol
mostViewedElements
	
	^ (storage sorted: [:a :b | a value >= b value]) collect: [:each | each key ]