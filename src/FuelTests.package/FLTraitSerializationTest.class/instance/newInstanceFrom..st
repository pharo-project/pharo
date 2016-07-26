running
newInstanceFrom: aTrait

	| aClass |
	aClass := self newClass.
	"If one wants to use a materialized trait in a class, it has to be added to Smalltalk globals. Fuel does not do that. Fuel just materializes the trait. It is up to the user to decide what to do with it.  "
	Smalltalk globals at: aTrait name put: aTrait.
	aClass addToComposition: aTrait.
	^ aClass new