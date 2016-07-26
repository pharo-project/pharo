private-hacks
addAbstractBinaryFileStream
	Smalltalk
		at: #AbstractBinaryFileStream
		ifPresent: [ :x | ^ self ].
		
	Stream subclass: #AbstractBinaryFileStream
		instanceVariableNames: '' 
		classVariableNames: '' 
		poolDictionaries: '' 
		category: self hacksCategoryName