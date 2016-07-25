initialize-release
close
	stream ifNotNil: [ 
		stream close. 
		stream := nil ]