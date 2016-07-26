private
printObjectAsAnItem: anObject
	| header printString |
	header := (anObject class name first isVowel
		ifTrue: [ 'an ' ]
		ifFalse: [ 'a ' ]) , anObject class name.
	printString := (GTObjectPrinter new asTruncatedTextFrom: anObject) asString.
	^ (printString beginsWith: header)
		ifTrue: [ printString ]
		ifFalse: [ 
			String streamContents: [ :out | 
				out << header; << ' ('; << printString; << ')' ] ]