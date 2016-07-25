private
writeContentsSafelyFromFile: osPath on: aStream
	aStream nextPutAll:
		([
			(FileStream readOnlyFileNamed: osPath) upToEnd
		 ] on: Error do: [:ex| ex return: ex printString])