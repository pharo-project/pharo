printing
printOn: aStream
	modifiers
		do: [ :modifier| modifier printOn: aStream ]
		separatedBy: [ aStream << ' + ' ].