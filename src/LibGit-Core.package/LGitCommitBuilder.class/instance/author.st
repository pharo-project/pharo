accessing
author
	^ author ifNil: [
		author := self repository defaultSignature ]