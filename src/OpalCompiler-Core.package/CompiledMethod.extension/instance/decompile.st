*opalcompiler-core
decompile
	^Smalltalk globals 
		at: #FBDDecompiler
		ifPresent: [ :decompiler | decompiler new  decompile: self]  
		ifAbsent: [ self selector asMethodPreamble, '
	self noSourceAvailable' ]