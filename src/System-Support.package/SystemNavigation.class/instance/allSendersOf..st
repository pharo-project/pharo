message sends
allSendersOf: selector 
	|  senders |
	senders := OrderedCollection new.
	self allBehaviorsDo: [ :behavior | 
		(behavior thoroughWhichSelectorsReferTo: selector) do: [ :sel| 
			senders add: (self createMethodNamed: sel realParent: behavior) ] ].
	^ senders