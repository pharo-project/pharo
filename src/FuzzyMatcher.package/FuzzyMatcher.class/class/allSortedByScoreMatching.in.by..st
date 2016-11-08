utilities api
allSortedByScoreMatching: aPattern in: aCollection by: aBlockReturningString

	| matcher matches |
	
	aPattern isEmpty ifTrue: [ ^ aCollection asArray ].
	
	matcher := self pattern: aPattern.
	matches := OrderedCollection new: aCollection size // 2.
	
	aCollection do: [ :each | 
		matcher 
			match: (aBlockReturningString value: each) 
			ifScored: [ :score | matches add: score -> each ] 
	].
	
	matches sort: [ :a :b | a key >= b key ].
	
	^ matches collect: [ :each | each value ] as: Array
	