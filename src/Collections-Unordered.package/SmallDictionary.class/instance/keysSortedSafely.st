accessing
keysSortedSafely
	"Answer a SortedCollection containing the receiver's keys."
	| sortedKeys |
	sortedKeys := SortedCollection new: self size.
	sortedKeys sortBlock:
		[:x :y |  "Should really be use <obj, string, num> compareSafely..."
		((x isString and: [y isString])
			or: [x isNumber and: [y isNumber]])
			ifTrue: [x < y]
			ifFalse: [x class == y class
				ifTrue: [x printString < y printString]
				ifFalse: [x class name < y class name]]].
	self keysDo: [:each | sortedKeys addLast: each].
	^ sortedKeys reSort