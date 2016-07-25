private-hacks
addContext
	Smalltalk
		at: #Context
		ifPresent: [ :x | ^ self ].
		
	Object 
		subclass: #Context
		instanceVariableNames: '' 
		classVariableNames: '' 
		poolDictionaries: '' 
		category: self hacksCategoryName