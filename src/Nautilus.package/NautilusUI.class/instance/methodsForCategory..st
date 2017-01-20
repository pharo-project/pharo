widget method
methodsForCategory: aCategory
	| methods class env |
	class := self selectedClass.
	class ifNil: [ ^ #() ].
	env := self browsedEnvironment.
	methods := env isSystem
		ifTrue: [ self methodsInSystemEnvironmentForCategory: aCategory in: class ]
		ifFalse: [ self methodsInARestrictedEnvironment: env forCategory: aCategory in: class ].
	^ methods sort: self methodSortBlock.