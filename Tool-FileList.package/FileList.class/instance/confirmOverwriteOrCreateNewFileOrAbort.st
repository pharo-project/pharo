private
confirmOverwriteOrCreateNewFileOrAbort
	| choice  chooseList |
	chooseList := {'overwrite that file' translated.
	               'choose another name' translated . 
	               'cancel' translated. }.
	choice := UIManager default
		chooseFrom: chooseList
		title: 'Save contents' translated.
	^ choice