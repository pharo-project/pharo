error handling
parseErrorNode: aMessageString
	| sourceString |
	sourceString := stream contents copyFrom: self errorPosition to: stream contents size.
	^ RBParseErrorNode
		errorMessage: aMessageString value: sourceString at: self errorPosition