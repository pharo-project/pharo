removing
removeKey: key ifAbsent: aBlock
	"Remove key (and its associated value) from the receiver. If key is not in 
	the receiver, answer the result of evaluating aBlock. Otherwise, answer 
	the value externally named by key."

	| index result assoc |
	index := self findElementOrNil: key.
	assoc := array at: index.
	(assoc == nil or: [assoc key isNil]) ifTrue: [ ^ aBlock value ].
	assoc key: nil.
	result := assoc value.
	assoc expire.
	expired := expired + 1.
	tally := tally - 1.
	^result