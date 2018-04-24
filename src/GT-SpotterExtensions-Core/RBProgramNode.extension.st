Extension { #name : #RBProgramNode }

{ #category : #'*GT-SpotterExtensions-Core' }
RBProgramNode >> gtSpotterCodePreviewIn: aComposite [
	<spotterPreview: 10>
	self source ifNil: [ ^ self ].
		
	^ aComposite pharoMethod
			title: [ 'Source code' translated ];
			display: [ self source ];
			smalltalkClass: [ self parents first compilationContext ifNotNil: [ :ctx | ctx getClass ] ];
			selectionInterval: [ self sourceInterval ];
			entity: self
]
