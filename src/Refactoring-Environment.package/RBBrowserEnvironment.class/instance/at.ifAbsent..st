accessing-classes
at: aKey ifAbsent: aBlock
	| association |
	association := self
		associationAt: aKey 
		ifAbsent: [ nil ].
	^ association isNil
		ifTrue: [ aBlock value ]
		ifFalse: [ association value ]