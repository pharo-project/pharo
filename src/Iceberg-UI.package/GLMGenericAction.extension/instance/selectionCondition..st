*Iceberg-UI
selectionCondition: selectionCondition
	"selectionCondition can be anything that can respond #cull: such as a block or unary symbol.
	It will only be evaluated if the presentation has a (not nil) selected element."
	
	^ self enabledCondition: [ :presentation | 
		presentation selection notNil and: [ 
			selectionCondition cull: presentation selection ]]