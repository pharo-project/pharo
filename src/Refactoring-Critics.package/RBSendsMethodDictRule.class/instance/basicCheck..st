running
basicCheck: aMethod
	({Behavior. ClassDescription. Class.TBehavior . TClassDescription . TApplyingOnClassSide } includes: aMethod methodClass)
		ifTrue: [ ^ false ].
	^ (aMethod messages includes: #methodDict)
		