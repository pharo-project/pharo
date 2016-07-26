menus behavior
removeMethod: aMethod inClass: aClass
	"If a message is selected, create a Confirmer so the user can verify that  
	the currently selected message should be removed from the system. If 
	so, remove it. "
	
	| result |
	self okToChange ifFalse: [^ false].
	result := super removeMethod: aMethod inClass: aClass.
	result ifTrue: [ self updateCategoryAndMethodList ].
	^result