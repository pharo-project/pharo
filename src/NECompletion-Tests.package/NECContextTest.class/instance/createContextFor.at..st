private
createContextFor: aString at: anInteger 
	^ NECContext
		controller: NECController new
		class: NECTestClass
		source: aString
		position: anInteger