printing
isSelfEvaluating

	^self methodClass notNil and: [(#(#DoIt #DoItIn: nil) includes: self selector) not]