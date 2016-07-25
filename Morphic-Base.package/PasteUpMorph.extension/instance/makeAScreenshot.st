*Morphic-Base
makeAScreenshot
	| filePrefix |
	filePrefix :=  'PharoScreenshot'.
	(UIManager default
		chooseFrom: {'The entire world' translated. 'A selected area' translated}
		values: #(#world #area )
		message: 'What do you want to shoot? (File will be saved in image directory)' translated
		title: 'Make a screenshot')
		ifNotNil: [:choice | 
				|form name|
				form := choice = #world
							ifTrue: [World imageForm]
							ifFalse: [Form fromUser].		
				name := (FileSystem workingDirectory / filePrefix , 'png') nextVersion.					
				PNGReadWriter
						putForm: form
						onFileNamed: (FileSystem workingDirectory / filePrefix , 'png') nextVersion.
				UIManager default inform: 'Screenshot saved under ', name fullName ] 
							
		