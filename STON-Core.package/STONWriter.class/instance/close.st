initialize-release
close
	writeStream ifNotNil: [
		writeStream close.
		writeStream := nil ]