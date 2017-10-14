tests
testSymbolIdentifier
	| passed |
	passed := false.
	
	[announcer 
		when: #FOO send: #bar to: nil;
		announce: #FOO ] 
		on: MessageNotUnderstood 
		do: [ :ex | passed := (ex message selector = #bar). ].
		
	self assert: passed 