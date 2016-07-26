possible bugs
sendsDifferentSuper
	| detector |
	detector := self new.
	detector name: 'Sends different super message'.
	detector methodBlock: 
			[:context :result | 
			| message |
			(message := context superMessages detect: [:each | each ~= context selector]
						ifNone: [nil]) notNil ifTrue: 
					[result addSearchString: message.
					result addClass: context selectedClass selector: context selector]].
	^detector