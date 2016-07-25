unnecessary code
implementedNotSent
	| detector |
	detector := self new.
	detector name: 'Methods implemented but not sent'.
	detector methodBlock: 
			[:context :result | 
			context compiledMethod senders
				senders: [result addClass: context selectedClass selector: context selector]].
	^detector