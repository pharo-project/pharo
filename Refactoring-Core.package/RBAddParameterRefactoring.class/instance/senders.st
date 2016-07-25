private
senders
	senders isNil 
		ifTrue: 
			[senders := Set new.
			self model allReferencesTo: oldSelector
				do: [:each | senders add: each modelClass]].
	^senders