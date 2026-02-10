Extension { #name : 'ScalingCanvas' }

{ #category : '*OSWindow-Morphic' }
ScalingCanvas class >> example [

	| morph |

	(morph := MenuMorph new)
		addTitle: 'Lorem ipsum'.
	(morph add: 'Dolor sit amet' target: nil selector: #yourself) keyText: 'x';
		icon: (self iconNamed: #smallConfiguration).
	(morph add: 'Consectetur adipiscing elit' target: nil selector: #yourself) keyText: 'y'.
	morph addLine.
	(morph add: 'Sed do eiusmod' target: nil selector: #yourself) keyText: 'z'.
	
	^ self privateExampleWithMorph: morph
]

{ #category : '*OSWindow-Morphic' }
ScalingCanvas class >> exampleImageMorph1 [

	^ self privateExampleWithMorph: (Morph new
		color: Color veryVeryLightGray;
		addMorph: ((ImageMorph withForm: (self iconNamed: #search))
			position: 4@4;
			yourself);
		addMorph: ((ImageMorph withForm: (self iconNamed: #search) copy)
			position: 24@4;
			yourself);
		extent: 44@24;
		yourself)
]

{ #category : '*OSWindow-Morphic' }
ScalingCanvas class >> exampleImageMorph2 [

	| forms imageMorph |
	
	forms := (1 to: 2) collect: [ :scale |
		(FormCanvas extent: 100 asPoint * scale depth: 1)
			fillOval: ((0 asPoint extent: 100 asPoint * scale) insetBy: 5 * scale) fillStyle: Color green;
			form ].
	(imageMorph := ImageMorph withFormSet: (FormSet forms: forms))
		color: Color blue.
	^ self privateExampleWithMorph: imageMorph
]

{ #category : '*OSWindow-Morphic' }
ScalingCanvas class >> exampleImageMorph3 [

	| forms imageMorph |
	
	forms := (1 to: 2) collect: [ :scale |
		(FormCanvas extent: 10 asPoint * scale)
			fillOval: ((0 asPoint extent: 10 asPoint * scale) insetBy: 1 * scale) fillStyle: Color black;
			form ].
	(imageMorph := ImageMorph withFormSet: (FormSet forms: forms))
		resize: 200@100.
	^ self privateExampleWithMorph: imageMorph
]
