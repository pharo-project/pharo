testing
shouldEnableFullStackButton

	self debugger ifNil: [ ^ false ].  
		
	^ self debugger stack listItems 
		ifEmpty: [ false ] 
		ifNotEmpty: [ :aList |
			aList last sender notNil ] 