testing
isDevelopment
	^  (self project hasVersion: #development) and: [
		(self project version: #development) = self version ]