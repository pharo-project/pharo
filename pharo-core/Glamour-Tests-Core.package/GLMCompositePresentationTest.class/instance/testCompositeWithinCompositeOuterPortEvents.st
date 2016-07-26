as yet unclassified
testCompositeWithinCompositeOuterPortEvents
	| childComposite finder parentComposite count |
	count := 0.
	childComposite := GLMCompositePresentation new.
	finder := childComposite finder.
	finder show: [ :a | a list display: [ :x | 1 to: x ] ].
	finder
		when: GLMPaneAdded
		do: [ :a | count := count + 1 ].
	parentComposite := GLMCompositePresentation new.
	parentComposite custom: childComposite.
	parentComposite startOn: 10.
	self assert: count equals: 1