util
newCommand
	" 
	MBCommand newCommand
	"
	| command baseName newCommandName receiverClassIndex clss title documentation |
	baseName :=  self uiManager request: 'Enter the base name (''MB'' and ''Command'' will be automatically added'.
	baseName ifNil: [ ^ self ].
	
	clss := MBCommand subclasses.
	receiverClassIndex := self uiManager chooseFrom: (clss collect: #name).
	receiverClassIndex isZero ifTrue: [ ^ self ].
	
	newCommandName := 'MB', baseName, 'Command'.
	command := 
		(clss at: receiverClassIndex) 
			subclass: newCommandName asSymbol 
			instanceVariableNames: '' 
			classVariableNames: '' 
			poolDictionaries: '' 
			category: self category.
	
	documentation := self uiManager request: 'Enter the documentation'.
	documentation ifNil: [ ^ self ].
	 command compile: 'documentation
	^ ''', documentation, '''' classified: 'documentation'.

	 command compile: 'execute
	self subclassResponsibility ' classified: 'execute'.
	
	title := self uiManager request: 'Enter the command title'.
	title ifNil: [ ^ self ].
	 command compile: 'title
	^ ''', title, '''' classified: 'accessing'.
	
	clss := MBInfo allSubclasses.
	receiverClassIndex := self uiManager chooseFrom: (clss collect: #name).
	receiverClassIndex isZero ifTrue: [ ^ self ].
	(clss at: receiverClassIndex) compile: 'cmd', baseName, '
	^ ', newCommandName classified: 'commands'