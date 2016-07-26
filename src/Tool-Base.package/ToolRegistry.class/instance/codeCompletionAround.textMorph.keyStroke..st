misc
codeCompletionAround: aBlock textMorph: aTextMorph keyStroke: evt
	^(self hasToolNamed: #codeCompletion)
	ifTrue: [ self codeCompletion codeCompletionAround: aBlock textMorph: aTextMorph keyStroke: evt ]
	ifFalse: [ aBlock value ]