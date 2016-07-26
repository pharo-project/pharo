accessing-classes
associationAt: aKey ifAbsent: aBlock
	| association class |
	association := self systemDictionary
		associationAt: aKey
		ifAbsent: [ ^ aBlock value ].
	class := association value isBehavior
		ifTrue: [ association value ]
		ifFalse: [ association value class ].
	^ ((self includesClass: class) or: [ self includesClass: class class ])
		ifTrue: [ association ]
		ifFalse: [ nil ]