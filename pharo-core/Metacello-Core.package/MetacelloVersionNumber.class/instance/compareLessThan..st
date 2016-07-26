private
compareLessThan: aMetacelloVersionNumber

	| mySize aSize commonSize count more |
	mySize := self size.
	aSize := aMetacelloVersionNumber size.
	commonSize :=  mySize min: aSize.
	count := 0.
	more := true.
	[ more and: [ count < commonSize ]] whileTrue: [
		(self at: count + 1) = (aMetacelloVersionNumber at: count + 1)
			ifTrue: [ count := count + 1 ]
			ifFalse: [ more := false ]].
	count < commonSize
		ifTrue: [ 
			^(self at: count + 1) 
				metacelloVersionComponentLessThan: (aMetacelloVersionNumber at: count + 1) ].
	mySize < aSize
		ifTrue: [ 
			mySize = 0 ifTrue: [ ^true ].
			"if the versions at commonSize are equal and the next version slot in aMetacelloVersionNumber 
			 is a string, then it's considered that I'm > aMetacelloVersionNumber
			 (i.e., '2.9.9' is greater than '2.9.9-alpha.2')"
			(self at: commonSize) = (aMetacelloVersionNumber at: commonSize)
				ifFalse: [ ^true ]. 
			^(aMetacelloVersionNumber at: commonSize+1) isString not]
		ifFalse: [ 
			mySize = aSize ifTrue: [ ^false ].
			aSize <= 0 ifTrue: [ ^false ].
			"if the versions at commonSize are equal and the next version slot is a string, 
			 then it's considered that I'm < aMetacelloVersionNumber
			 (i.e., '2.9.9-alpha.2' is less than '2.9.9')"
			(self at: commonSize) = (aMetacelloVersionNumber at: commonSize)
				ifFalse: [ ^false ].
			 ^(self at: commonSize+1) isString]