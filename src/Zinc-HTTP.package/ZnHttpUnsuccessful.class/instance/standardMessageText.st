accessing
standardMessageText
	"Generate a standard textual description"
	
	^ String streamContents: [ :stream |
		response ifNotNil: [
			response statusLine printCodeAndReasonOn: stream ] ]