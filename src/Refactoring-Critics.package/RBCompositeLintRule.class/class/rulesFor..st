instance creation
rulesFor: aRuleClass
	| rules |
	rules := SortedCollection
		sortBlock: [ :a :b | a name <= b name ].
	aRuleClass withAllSubclassesDo: [ :each |
		each isVisible
			ifTrue: [ rules add: each new ] ].
	^ rules asArray