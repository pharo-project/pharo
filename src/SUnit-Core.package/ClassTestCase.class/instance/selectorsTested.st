coverage
selectorsTested
	| literals |
	literals := Set new.
	self class
		selectorsAndMethodsDo: [ :s :m | (s beginsWith: 'test')
			ifTrue: [ literals addAll: (m messages)] ].
	^ literals asArray sort