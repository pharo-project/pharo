testing
isValidGlobalName

	self ifEmpty: [ ^ false ].
	
	"reserverd default names"
	self = 'NameOfSubclass' ifTrue: [ ^ false ].
	self = 'TNameOfTrait' ifTrue: [ ^ false ].
	
	^ Character supportsNonASCII 	
		ifTrue: [ 
			(self first isLetter 
				and: [self first isUppercase]) 
				and: [ self allSatisfy: [:character | 
						character isAlphaNumeric or: [ character = $_ ]]]]		
		ifFalse: [ 
			(self first between: $A and: $Z) and: [  
				self allSatisfy: [:character |
					(character between: $a and: $z) or: [
					(character between: $A and: $Z) or: [ 
					(character between: $0 and: $9) or: [
					character = $_]]]]]]
