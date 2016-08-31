private-scanning
stripComment
	| start stop |
	start := stream position.
	self step.
	characterType = #eof
		ifTrue: [ ^ self scanError: 'Unmatched " in comment.' ].
	[ currentCharacter = $" and: [ self step ~= $" ] ]
		whileFalse: [ characterType = #eof
				ifTrue: [ ^ self scannerError: 'Unmatched " in comment.' ].
			self step ].
	characterType = #eof
		ifTrue: [ stop := stream position ]
		ifFalse: [ stop := stream position - 1 ].
	comments add: (start to: stop)