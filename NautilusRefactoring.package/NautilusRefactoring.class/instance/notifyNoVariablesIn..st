private
notifyNoVariablesIn: aClass 
	self inform: aClass printString , ' does not define any instance variable.'.
	RefactoringAborted signal.