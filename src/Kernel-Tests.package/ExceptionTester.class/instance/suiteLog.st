accessing
suiteLog

	suiteLog == nil
		ifTrue: [suiteLog := OrderedCollection new].
	^suiteLog