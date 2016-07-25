initialization
initializeRule: aRule sourceAnchor: anAnchor method: aString condition: aCondition

	self initializeRule: aRule sourceAnchor: anAnchor.
	targetMethodName := aString.
	condition := aCondition 