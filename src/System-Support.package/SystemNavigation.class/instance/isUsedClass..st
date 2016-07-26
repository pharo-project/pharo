message sends
isUsedClass: aClass

	self allBehaviorsDo: [ :behavior | 
		(behavior hasSelectorReferringTo: aClass binding) ifTrue: [^true] ].
	^false