utilities
assureAll: selector areCategorizedIn: category whenSubclassOf: aClass
	| violating |
	violating := OrderedCollection new.
	aClass allSubclassesDo: [:cls | cls methods do: [:m |
			(m selector = selector and: [ m category ~= category ])
				ifTrue: [ violating add: m -> m category ]
			]
		 ].
	self assert: violating isEmpty description: ('Subclasses of {1} should have #{2} methods in category {3}' format: { aClass asString. selector. category })
