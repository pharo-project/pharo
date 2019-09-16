Extension { #name : #PasteUpMorph }

{ #category : #'*Morphic-Base' }
PasteUpMorph >> dragThroughOnDesktop: evt [
	"Draw out a selection rectangle"
	| selection |
	selection := SelectionMorph newBounds: (evt cursorPoint extent: 8@8).
	self addMorph: selection.
	^ selection extendByHand: evt hand

]

{ #category : #'*Morphic-Base' }
PasteUpMorph >> drawingClass [

	^ ImageMorph
]

{ #category : #'*Morphic-Base' }
PasteUpMorph >> makeAScreenshot [
	| filePrefix |
	filePrefix := 'PharoScreenshot'.
	(UIManager default
		chooseFrom: {'The entire world' translated . 'A selected area' translated}
		values: #(#world #area)
		message: 'What do you want to shoot? (File will be saved in image directory)' translated
		title: 'Make a screenshot')
		ifNotNil: [ :choice | 
			| form name |
			form := choice = #world ifTrue: [ self currentWorld imageForm ] ifFalse: [ Form fromUser ].
			name := (FileSystem workingDirectory / filePrefix , 'png') nextVersion.
			PNGReadWriter putForm: form onFileNamed: (FileSystem workingDirectory / filePrefix , 'png') nextVersion.
			UIManager default
				inform:
					(String
						streamContents: [ :s | 
							s
								nextPutAll: 'Screenshot saved under ';
								nextPutAll: name fullName;
								cr;
								nextPutAll: 'Click to open location' ])
				actionOnClick: [ name openInOSFileBrowser ] ]
]
