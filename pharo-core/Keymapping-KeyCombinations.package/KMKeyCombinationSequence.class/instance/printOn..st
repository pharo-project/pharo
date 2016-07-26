printing
printOn: aStream
	shortcuts 
		do: [ :shortcut| shortcut printOn: aStream]
		separatedBy: [ aStream nextPutAll: ' , '].