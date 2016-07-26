rendering
textModelFor: aPresentation
	| tm |
	tm := GLMSmalltalkCodeModel new
		text: aPresentation formatedDisplayValue;
		glamourPresentation: aPresentation;
		highlightSmalltalk: true;
		highlightSmalltalkContext: aPresentation highlightSmalltalkContext.
	self addVariableBindingsOn: tm from: aPresentation.
	aPresentation isForScripting ifTrue: [ 
		tm bindingsStrategy: self workspaceBindingsStrategy ].
	^ tm