preconditions
checkSendersAccessTo: name 
	| violatorClass |
	(#('self' 'super') includes: name) ifTrue: [^self].
	violatorClass := self senders 
				detect: [:each | (self canReferenceVariable: name in: each) not]
				ifNone: [nil].
	violatorClass notNil 
		ifTrue: 
			[self refactoringError: ('<1s> doesn''t appear to be defined in <2p>' 
						expandMacrosWith: name
						with: violatorClass)]