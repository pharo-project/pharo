private
privateReferencesTo: aSelector 
	| methods |
	methods := OrderedCollection new.
	self allBehaviorsDo: [ :class |
		(class whichSelectorsReferToSymbol: aSelector)
			do: [ :selector |
				methods add: (class methodFor: selector) ] ].
	^ methods