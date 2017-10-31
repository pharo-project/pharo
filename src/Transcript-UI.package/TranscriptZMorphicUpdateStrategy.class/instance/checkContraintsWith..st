accessing
checkContraintsWith: aBinaryInputBlock

	"[ :aCollection :aBlock | something here; must return a Boolean value! ]"
	
	"Examples:
		
		[ :aCollection :aBlock | aCollection anySatisfy: aBlock ]
		[ :aCollection :aBlock | aCollection noneSatisfy: aBlock ]
		
	"
	
	constraintsChecker := aBinaryInputBlock