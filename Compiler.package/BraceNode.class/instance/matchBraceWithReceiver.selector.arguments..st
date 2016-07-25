initialize-release
matchBraceWithReceiver: receiver selector: selector arguments: arguments

	selector = (self selectorForShortForm: arguments size)
		ifFalse: [^ nil "no match"].

	"Appears to be a short form brace construct"
	self elements: arguments