tests - dictionary key access
testKeyAtValue
	"self run: #testKeyAtValue"
	"self debug: #testKeyAtValue"
	
	| dict value result |
	dict := self nonEmpty .
	value := dict values anyOne.
	
	result := dict keyAtValue: value.
	self assert: (dict at: result) = value.
		
	self should: [dict keyAtValue: self valueNotIn ] raise: Error
	
	