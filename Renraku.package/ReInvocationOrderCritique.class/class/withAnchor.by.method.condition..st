instance creation
withAnchor: anAnchor by: aRule method: aString condition: aCondition

	^ self new
		initializeRule:	aRule
		  sourceAnchor: anAnchor
		        method: aString
		     condition: aCondition;
		
		yourself