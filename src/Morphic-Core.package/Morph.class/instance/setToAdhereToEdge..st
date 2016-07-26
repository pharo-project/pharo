menus
setToAdhereToEdge: anEdge
	anEdge ifNil: [^ self].
	anEdge == #none ifTrue: [^ self removeProperty: #edgeToAdhereTo].
	self setProperty: #edgeToAdhereTo toValue: anEdge.
