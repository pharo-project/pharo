testing
testUpload
	self withServerDo: [ :server |
		| image client |
		image := self image.
		client := ZnClient new.
		client url: server localUrl; addPath: #image.
		client addPart: (ZnMimePart fieldName: #file entity: image).
		client post.
		self assert: client isSuccess.
		client resetEntity; queryAt: #raw put: #true.
		client get.
		self assert: client isSuccess.
		self assert: client entity equals: image.
		client close ]