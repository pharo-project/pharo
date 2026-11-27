Extension { #name : 'Screenshot' }

{ #category : '*Morphic-Desktop' }
Screenshot >> makeAScreenshot [

	| filePrefix |
	filePrefix := 'PharoScreenshot'.
	(MorphicUIManager new
		 chooseFrom: {
				 'The entire world' translated.
				 'A selected area' translated }
		 values: #( #world #area )
		 message:
			 'What do you want to shoot? (File will be saved in image directory)'
				 translated
		 title: 'Make a screenshot') ifNotNil: [ :choice |
		| form name |
		form := choice = #world
			        ifTrue: [ self currentWorld imageForm ]
			        ifFalse: [ self class new formScreenshotFromUserSelection ].
		name := (FileSystem workingDirectory / filePrefix , 'png')
			        nextVersion.
		PNGReadWriter
			putForm: form
			onFileNamed:
			(FileSystem workingDirectory / filePrefix , 'png') nextVersion.

		(GrowlMorph openWithLabel: 'Information' contents: (String streamContents: [ :s |
								  s
									  nextPutAll: 'Screenshot saved under ';
									  nextPutAll: name fullName;
									  cr;
									  nextPutAll: 'Click to open location' ]))
				vanishDelay: 2 second;
				actionBlock: [ name openInOSFileBrowser ] ]
]
