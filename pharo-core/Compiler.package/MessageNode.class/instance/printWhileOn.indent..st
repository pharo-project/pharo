printing
printWhileOn: aStream indent: level

	self printReceiver: receiver on: aStream indent: level.
	(arguments isEmpty not
	 and: [arguments first isJust: NodeNil]) ifTrue:
			[selector := SelectorNode new
							key:
									(selector key == #whileTrue:
										ifTrue: [#whileTrue]
										ifFalse: [#whileFalse])
							code: #macro.
			arguments := Array new].
	self printKeywords: selector key arguments: arguments
		on: aStream indent: level