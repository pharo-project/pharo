instance creation
withAnchor: anAnchor by: aRule

	^ self basicNew
		initializeRule: aRule
		sourceAnchor: anAnchor;
		yourself