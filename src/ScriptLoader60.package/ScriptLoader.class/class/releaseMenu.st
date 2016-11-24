public
releaseMenu
	"self releaseMenu"
	|symbol|
	symbol := UIManager default
		chooseFrom: #('1- Prepare new update'
		                        '2- Save new image for testing (optional)'
						'3- Freeze changes: packages will be saved (required)'
						'4- Verify new update (open the mother image in the same folder) (optional)'
						'5- Publish changes')
		values: #(prepareNewUpdate saveImageForRunningTests doneApplyingChanges verifyNewUpdate publishChanges).
	symbol ifNotNil: [ScriptLoader new perform: symbol]