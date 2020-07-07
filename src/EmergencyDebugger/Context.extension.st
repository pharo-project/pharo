Extension { #name : #Context }

{ #category : #'*EmergencyDebugger' }
Context >> printWithArgsValueOn: aStream [
	| keywords |
	aStream << self method methodClass name.
	aStream << '>>'.
	self method numArgs = 0
		ifTrue: [ 
			"If self method is a compiled block, we ask its method.
			The method of a method is the method itself."
			aStream << self method method name.
			^ self ].
	keywords := self method selector keywords.
	1 to: keywords size do: [ :i | 
		| keyword argName argValue |
		keyword := keywords at: i.
		argName := self method argumentNames at: i.
		self flag: 'Sometimes the arguments cannot be accessed. Why?'.
		argValue := [ self arguments at: i ]
			on: Error
			do: [ $? ].
		aStream << keyword.
		aStream space.
		aStream << argName.
		aStream space.
		aStream << '('.
		aStream << argValue printString.
		aStream << ')' ]
]
