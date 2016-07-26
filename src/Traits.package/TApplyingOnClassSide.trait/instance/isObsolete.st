testing
isObsolete
	"Return true if the receiver is obsolete"
	^self soleInstance == nil "Either no thisClass"
		or:[self soleInstance classSide ~~ self "or I am not the class of thisClass"
			or:[self soleInstance isObsolete]] "or my instance is obsolete"