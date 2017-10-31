running
check: aMethod forCritiquesDo: aCriticBlock	
	({Behavior. ClassDescription. Class.TBehavior . TClassDescription . TApplyingOnClassSide } includes: aMethod methodClass)
		ifTrue: [ ^ self ].
	(aMethod messages includes: #methodDict) ifTrue: [ 
		aCriticBlock cull: (self critiqueFor: aMethod) ]
		