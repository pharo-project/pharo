requests
shouldExtractAssignmentTo: aString 
	^(self options at: #extractAssignment) value: self value: aString