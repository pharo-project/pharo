accessing
classifier
	^ methodClassifier ifNil: [ methodClassifier := self class methodClassifierClass new ]