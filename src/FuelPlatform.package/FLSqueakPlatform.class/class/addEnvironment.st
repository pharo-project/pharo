private-hacks
addEnvironment
	"Don't name the category 'Fuel-Hacks'. That category will be nuked on load"	
	Smalltalk
		at: #Environment
		ifPresent: [ :x | ^ self ].
		
	Object 
		subclass: #Environment
		instanceVariableNames: '' 
		classVariableNames: '' 
		poolDictionaries: '' 
		category: self hacksCategoryName