widget method
methodsForCategory: aCategory 
	| methds class env |
	
	class := self selectedClass.
	class ifNil: [ ^ #() ].
	env := self browsedEnvironment.
	methds := env isSystem
		ifTrue: [ self methodsInSystemEnvironmentForCategory: aCategory  in: class ]
		ifFalse: [ self methodsInARestrictedEnvironment: env forCategory: aCategory  in: class ].
	^ methds sort: [:a :b | a selector < b selector ].