private
projectPlatformAttributes

	| list aBlock |
	list := OrderedCollection new.
	(aBlock := self projectAttributes) ~~ nil 
		ifTrue: [ list addAll: aBlock value ].
	^self defaultPlatformAttributes, list