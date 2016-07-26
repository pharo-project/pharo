tags
computeTagsFor: aGlamourValue from: aBlock
	| value |
	
	aBlock ifNil: [ ^ OrderedCollection new ].
	value := aBlock glamourValue: aGlamourValue.
	^ value isString
		ifTrue: [ OrderedCollection with: value ]
		ifFalse: [ value asOrderedCollection ]