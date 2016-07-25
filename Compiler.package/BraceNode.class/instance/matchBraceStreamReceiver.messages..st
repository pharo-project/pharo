initialize-release
matchBraceStreamReceiver: receiver messages: messages

	((receiver isMessage: #braceStream: receiver: nil arguments: [:arg | arg isConstantNumber])
		and: [messages last isMessage: #braceArray receiver: nil arguments: nil])
		ifFalse: [^ nil "no match"].

	"Appears to be a long form brace construct"
	self elements: (messages allButLast collect:
		[:msg | (msg isMessage: #nextPut: receiver: nil arguments: nil)
					ifFalse: [^ nil "not a brace element"].
		msg arguments first])