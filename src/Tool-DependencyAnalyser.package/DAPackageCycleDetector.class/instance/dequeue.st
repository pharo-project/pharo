actions
dequeue
	| aNode |
	aNode := queue nextOrNil.
	aNode ifNotNil: [ aNode inStack: false ].
	^ aNode