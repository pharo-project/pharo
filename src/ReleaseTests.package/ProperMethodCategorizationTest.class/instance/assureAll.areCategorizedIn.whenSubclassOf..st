utilities
assureAll: selector areCategorizedIn: protocol whenSubclassOf: aClass
	| violating |
	violating := OrderedCollection new.
	aClass allSubclassesDo: [:cls | cls methods do: [:m |
			(m selector = selector and: [ m category ~= protocol ])
				ifTrue: [ violating add: m -> m category ]
			]
		 ].
	self assert: violating isEmpty description: [self describeViolations: violating forSubclassOf: aClass protocol: protocol]
