testing
testProgressNoIfFail
	self 
		should: [ 
			ZnClient new
				beOneShot;
				signalProgress: true;
				ifFail: [ self fail ];
				get: self smallHtmlUrl ] 
		raise: HTTPProgress
