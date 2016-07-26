instance creation
fromArray: anArray
	| configuration |
	configuration := self new.
	anArray pairsDo: [:key :value |
		key = #repository
			ifTrue: [configuration repositories add: (self repositoryFromArray: value)].
		key = #dependency
			ifTrue: [configuration dependencies add: (self dependencyFromArray: value)].
	].
	^configuration