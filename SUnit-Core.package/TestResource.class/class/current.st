accessing
current
	"This is a lazy accessor:  the assert of self isAvailable does no work unless current isNil.  However this method should normally be sent only to a resource that should already have been made available, e.g. in a test whose test case class has the resource class in its #resources, so should never be able to fail the assert.
	If the intent is indeed to access a possibly-unprepared or reset-in-earlier-test resource lazily, then preface the call of 'MyResource current' with 'MyResource availableFor: self'."
	
	self
		assert: self isAvailable
		description:
			'Sent #current to unavailable resource ' , self name ,
					'.  Add it to test case'' class-side #resources (recommended) or send #availableFor: beforehand'.
	^current