running
setUpPackageClassDefs: classDefArray named: pName ancestors: ancestors
	"self reset"
	
	^self setUpPackageClassDefs: classDefArray methodDefs: #() named: pName ancestors: ancestors
