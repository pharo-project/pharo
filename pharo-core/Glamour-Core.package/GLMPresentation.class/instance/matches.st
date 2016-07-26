testing
matches 
	<debuggingStone: #presentationCondition label: 'Presentation condition'>
	"answer true iff the presentation matches the current context so that it can be displayed"
	^ (self usesImplicitAllNil
	    ifTrue:[ GLMAllNilCondition new glamourValue: self entity ]
	    ifFalse:[ self usesImplicitNotNil 
			ifTrue: [GLMAllNotNilCondition new glamourValue: self entity] 
			ifFalse: [GLMSomeNotNilCondition new glamourValue: self entity] ]
		) and: [
			(self condition glamourValue: self entity) = true]