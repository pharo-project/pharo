printing
prettyPrintOn: aStream

	shortcuts 
		do: [ :e | e prettyPrintOn: aStream ]
		separatedBy: [ aStream << ' + ' ]