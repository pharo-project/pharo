rendering
modelFor: aPresentation

	^ GLMRubricSmalltalkTextModel new
		glamourPresentation: aPresentation;
		highlightSmalltalk: true;
		highlightSmalltalkContext: aPresentation highlightSmalltalkContext;
		yourself