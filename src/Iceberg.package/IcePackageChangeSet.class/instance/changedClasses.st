initialization
changedClasses
	^ (self changes collect: #iceClass as: Set) 
		sorted: [ :a :b | a description > b description ]