accessing
environmentFor: aPlatform
	| environmentClass |
	
	environmentClass := self allSubclasses
		detect: [ :each | each isDefaultFor: aPlatform ]
		ifNone: [ nil ].
		
	^ (environmentClass notNil and: [ environmentClass isAvailable  ])  
		ifTrue: [ environmentClass platform: aPlatform ]
		ifFalse: [ PlatformIndependentEnvironment platform: aPlatform ]