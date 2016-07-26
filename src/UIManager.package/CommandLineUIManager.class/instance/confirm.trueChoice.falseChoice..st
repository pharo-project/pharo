ui requests
confirm: queryString trueChoice: trueLabel falseChoice: falseLabel
	^self
		confirm: queryString
		trueChoice: trueLabel
		falseChoice: falseLabel
		cancelChoice: nil
		default: nil
		