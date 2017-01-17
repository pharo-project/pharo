accessing
manifestOf: elem

	| packageName |
	
	packageName := elem package 
		ifNil: [ ^ nil ] 
		ifNotNil: [ :pkg | pkg name ].

	manifestClass := self class allManifestClasses 
		detect: [ :each | each category = packageName  ]
		ifNone: [ nil ].
	^ manifestClass
		
