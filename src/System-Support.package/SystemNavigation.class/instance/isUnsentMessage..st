message sends
isUnsentMessage: selector

	self allBehaviorsDo: [ :behavior | 
		(behavior thoroughHasSelectorReferringTo: selector) ifTrue: [^false] ].
	^true