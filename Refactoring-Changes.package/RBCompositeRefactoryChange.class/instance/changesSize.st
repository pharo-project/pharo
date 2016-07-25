accessing
changesSize
	^ changes inject: 0 into: [ :sum :each | sum + each changesSize ]