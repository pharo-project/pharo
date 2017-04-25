rendering
modelFor: aPresentation
	| highlightContext |
	highlightContext := aPresentation highlightSmalltalkContext. 
	^ GLMRubricSmalltalkTextModel new
		glamourPresentation: aPresentation;
		highlightSmalltalk: highlightContext notNil;
		highlightSmalltalkContext: highlightContext;
		yourself