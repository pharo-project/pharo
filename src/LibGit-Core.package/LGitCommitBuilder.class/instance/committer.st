accessing
committer
	^ committer ifNil: [ 
		committer := self repository defaultSignature ]