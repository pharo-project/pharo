accessing
stringColor
	| size |
	size := self textSize.
	self warningLimit negative ifTrue: [ ^ Color black ].
	^ size >= self warningLimit
			ifTrue: [
				size >= self alertLimit
					ifTrue: [ Color red ]
					ifFalse: [ Color orange ]]
			ifFalse: [ Color black ].