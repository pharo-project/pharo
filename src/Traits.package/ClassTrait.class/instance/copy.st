copying
copy
	"Make a copy of the receiver. Share the 
	reference to the base trait."

	<localClassTraitMethod> 
    "This method is supposed to be local in ClassTrait because of a good reason.
    We use this pragma to test if ClassTrait does not contain some accidental 
    local selectors."

	^(self class new)
		baseTrait: self baseTrait;
		initializeFrom: self;
		yourself