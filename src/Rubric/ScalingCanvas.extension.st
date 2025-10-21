Extension { #name : 'ScalingCanvas' }

{ #category : '*Rubric' }
ScalingCanvas class >> exampleRubScrolledTextMorph [

	| source |

	source := String streamContents: [ :stream |
		stream nextPutAll: 'OrderedCollection new'; cr.
		(1 to: 100) groupsOf: 5 atATimeDo: [ :numbers |
			stream tab; nextPutAll: ('addAll: {1};' format: { numbers printString }); cr ].
		stream tab; nextPutAll: 'yourself'; cr ].
	^ self privateExampleWithMorph:
		(RubScrolledTextMorph new
			model: (RubScrolledTextModel new text: source; yourself);
			beForSmalltalkCode;
			withCodeSizeFeedback;
			extent: 320@200;
			yourself)
]

{ #category : '*Rubric-Editing-Core' }
ScalingCanvas >> rubParagraph: paragraph bounds: bounds color: c [

	| clippedCanvas currentParagraph |
	
	clippedCanvas := self copyClipRect: bounds.
	currentParagraph := paragraph.
	[ currentParagraph next isNil ] whileFalse: [
		(currentParagraph canDrawDecoratorsOn: clippedCanvas) ifFalse: [ ^ self ].
		currentParagraph drawOn: clippedCanvas.
		currentParagraph := currentParagraph next ].
	formCanvas rubParagraph: currentParagraph bounds: (bounds scaleBy: scale) color: c scale: scale
]
