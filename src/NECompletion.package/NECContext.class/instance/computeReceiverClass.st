private
computeReceiverClass
	| previous found |
	recurseCount := 0.
	completionIndex = 1 ifTrue: [^nil].
	found := self checkImpossibleReceiver.
	found ifTrue: [^nil].
	previous := ranges at: completionIndex - 1.
	previous type = #cascadeSeparator 
		ifTrue: [^self guessCascadeReceiver: completionIndex - 1].
	(previous type = #unary 
		and: [(self sourceOf: previous) = 'new' and: [completionIndex > 2]]) 
			ifTrue: 
				[previous := ranges at: completionIndex - 2.
				previous type = #globalVar 
					ifTrue: [^self guessTypeOf: completionIndex - 2]
					ifFalse: 
						[self configureSelectorsOnly.
						^nil]].
	^self guessTypeOf: completionIndex - 1