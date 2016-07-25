instance creation
rule: aRule achor: anAnchor oldNode: aNode newNode: anotherNode

	^ self basicNew 
		initializeRule: aRule
		sourceAnchor: anAnchor
		oldNode: aNode
		newNode: anotherNode