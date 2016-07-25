*renraku
check: anEntity ifNoCritiques: alternativeBlock
	| critics |
	
	critics := OrderedCollection new.
	self check: anEntity forCritiquesDo: [ :critic | critics add: critic ] ifNone: alternativeBlock.
	^ critics