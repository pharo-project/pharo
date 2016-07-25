tests - dictionary key access
testKeyAtValueIfAbsent
	"self run: #testKeyAtValue"
	"self debug: #testKeyAtValue"
	
	| dict value result |
	dict := self nonEmpty .
	value := dict values anyOne.
	
	result := dict keyAtValue: value ifAbsent: [nil].
	self assert: (dict at: result) = value.
		
	self assert: (dict keyAtValue: self valueNotIn ifAbsent: [nil] ) = nil. 
	
	