convenient accesses
allTraits
	"convenient method"
	
	^self allClasses select:[ :each | each isTrait ]