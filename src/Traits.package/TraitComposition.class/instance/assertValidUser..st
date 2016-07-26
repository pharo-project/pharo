error-handling
assertValidUser: aBehavior
	"Assert that this trait composition set for aBehavior
	does not introduce a cycle."
	
	(self allTraits includes: aBehavior) ifTrue: [
		TraitCompositionException signal: 'Cycle in compositions:  The composition (in)directly includes this trait!']