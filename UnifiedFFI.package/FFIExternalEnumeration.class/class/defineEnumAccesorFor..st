private
defineEnumAccesorFor: enumName
	"Define accessors as class methods"
	| code |
	
	code := enumName, '
	"This method was automatically generated"
	^ ', enumName.
	
	self class 
		compile: code 
		classified: self generatedEnumProtocolName.