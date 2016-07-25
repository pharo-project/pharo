testing
isStable
	^ (self project hasVersion: #stable) and: [ 
		(self project version: #stable) = self version ]