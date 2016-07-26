accessing class hierarchy
classesThatImplementAllOf: selectorSet
	"Return an array of any classes that implement all the messages in selectorSet."

	| found remaining |
	found := OrderedCollection new.
	selectorSet do:
		[:sel | (self includesSelector: sel) ifTrue: [found add: sel]].
	found isEmpty
		ifTrue: [^ self subclasses inject: Array new
						into: [:subsThatDo :sub |
							subsThatDo , (sub classesThatImplementAllOf: selectorSet)]]
		ifFalse: [remaining := selectorSet copyWithoutAll: found.
				remaining isEmpty ifTrue: [^ Array with: self].
				^ self subclasses inject: Array new
						into: [:subsThatDo :sub |
							subsThatDo , (sub classesThatImplementAllOf: remaining)]]