Extension { #name : 'Behavior' }

{ #category : '*Ring-Definitions-Core' }
Behavior >> methodNamed: aSelector [
	<reflection: 'Class structural inspection - Selectors and methods inspection'>
	^ self methodDict at: aSelector
]
