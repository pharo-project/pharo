tests
testRefactoring
	"Really hacky"

	| aClass newName nautilus windowsAfter windowsBefore anEntry |
	"Create class"
	aClass := classFactory newClass.
	newName := aClass name, 'Renamed'.

	"Rename the class, using Nautilus machinery"
	nautilus := Nautilus browsedEnvironment: Nautilus defaultBrowsedEnvironment.
	windowsBefore := self changesBrowserOpenedWindows.
	[ (NautilusRefactoring model: nautilus) renameClass: aClass ]
		on: ProvideAnswerNotification
		do: [ :n | n resume: newName ].
	windowsAfter := self changesBrowserOpenedWindows.
	self assert: (windowsAfter copyWithoutAll: windowsBefore) size = 1.
	windowsAfter anyOne window triggerOkAction.
	self assert: aClass name = newName.

	"Check EpMonitor logged it correctly"
	self assert: (self countLogEventsWith: EpBehaviorNameChange) equals: 1.

	anEntry := (self allLogEntriesWith: EpBehaviorNameChange) anyOne.

	self 
		assert: (anEntry tags includesKey: EpLog triggererReferenceKey) 
		description: 'It did not recognize that the code change was triggered by other event.'