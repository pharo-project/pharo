subclass creation - immediate
immediateSubclass: className instanceVariableNames: instvarNames 
	classVariableNames: classVarNames package: cat
	"
	An immediate subclass define a class for which its value is coded within the OOP itself (like SmallInteger in 32 bits). It is not meant to be used by non-experimented users.
	
	Immediates are objects that are stored in an object pointer using a tag to distinguish them from ordinary object pointers.  In v3 the only immediate is SmallInteger.  In 32-but spur there are SmallInteger and Character.  An implication of this is that in spur all Characters can be compared using #==.  In 64-bit Spur there is also SmallFloat64.  If a float's exponent is in the middle 8-bits of the 11-bit exponent range then it will be immediate.  If a float's exponent is outside of the middle 8-bits it will be boxed.
	"
	^self immediateSubclass: className
		instanceVariableNames: instvarNames
		classVariableNames: classVarNames
		poolDictionaries: ''
		package: cat