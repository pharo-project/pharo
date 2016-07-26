accessing
tag
	"Return an exception's tag value."

	^ tag 
		ifNil: [ self messageText ]
		ifNotNil: [ tag ]