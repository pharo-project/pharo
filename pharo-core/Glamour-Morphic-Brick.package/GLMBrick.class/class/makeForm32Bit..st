utils
makeForm32Bit: aForm
	aForm ifNil: [ ^ nil ].
	aForm depth = 32 ifTrue: [ ^ aForm ].
	aForm depth: 32.
	aForm fixAlpha.
	^ aForm.