morphic ui
buttonSpecs
	repository readonly
		ifTrue: [ ^ super buttonSpecs ]
		ifFalse: [ ^ #(#('Pull' #gitPull 'git pull and refresh') #('Push' #gitPush 'git push' #gitNeedPush)) , super buttonSpecs ]