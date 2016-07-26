*MonticelloGUI
fillInTheBlankConfigure: aTemplateString 
	| chunk repo |
	aTemplateString
		ifNil: [^ false].
	chunk := UIManager default
				multiLineRequest: self fillInTheBlankRequest
				initialAnswer: aTemplateString
				answerHeight: 150.
	(chunk notNil and: [ chunk notEmpty ])
		ifTrue: [repo := self readFrom: chunk readStream.
			repo creationTemplate: chunk].
	^ repo