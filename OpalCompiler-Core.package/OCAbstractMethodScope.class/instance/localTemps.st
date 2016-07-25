temp vars
localTemps
	"all temps accessed in the context... for tempVectors, it takes all the vars even those not used here"
	| localVars |
	localVars := OrderedCollection new.
	self copiedVars do:  [:var |
		var isStoringTempVector 
			ifTrue: [var tempVectorForTempStoringIt do: [:tempVectorVars | localVars add: tempVectorVars]]
	].
	self tempVars do:  [:var | localVars add: var].
	^localVars asArray