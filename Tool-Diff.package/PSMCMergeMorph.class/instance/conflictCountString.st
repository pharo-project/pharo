as yet unclassified
conflictCountString
	"Answer a string describing the number of conflicts."

	|count|
	count := self conflictCount.
	^count = 1
		ifTrue: ['1 conflict' translated]
		ifFalse: ['{1} conflicts' translated format: {count}]