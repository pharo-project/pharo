tests - dictionary key access
testKeyAtIdentityValueIfAbsent
	"self run: #testKeyAtValue"
	"self debug: #testKeyAtValue"
	
	| dict value result |
	dict := self nonEmpty .
	value := dict values anyOne.
	
	result := dict keyAtIdentityValue: value ifAbsent: [nil].
	self assert: (dict at: result) = value.
		
	self assert: (dict keyAtIdentityValue: self valueNotIn ifAbsent: [nil] ) = nil. 
	