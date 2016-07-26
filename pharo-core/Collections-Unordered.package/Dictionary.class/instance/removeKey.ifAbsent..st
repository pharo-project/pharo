removing
removeKey: key ifAbsent: aBlock 
	"Remove key (and its associated value) from the receiver. If key is not in 
	the receiver, answer the result of evaluating aBlock. Otherwise, answer 
	the value externally named by key."

	| index assoc |
	index := self findElementOrNil: key.
	assoc := array at: index.
	assoc == nil ifTrue: [ ^ aBlock value ].
	array at: index put: nil.
	tally := tally - 1.
	self fixCollisionsFrom: index.
	^ assoc value