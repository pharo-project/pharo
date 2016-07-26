stream handling
onAmountChanged: anInteger in: aProcessor
	"Amount must be non-negative"
	self assert: anInteger >= 0.
	
	"If amount is not positive there is no need to do anything"
	anInteger = 0 ifTrue: [ ^ self ].
	self announcer announce: (GTSpotterCandidatesAmountChanged new amount: anInteger; category: aProcessor; yourself)