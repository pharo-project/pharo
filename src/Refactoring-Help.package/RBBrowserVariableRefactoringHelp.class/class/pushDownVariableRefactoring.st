pages
pushDownVariableRefactoring
	^HelpTopic
		title: 'Push down'
		contents: 
'
Moves an instance/class variable down to the subclasses. The variable is added to every direct subclass.
A warning dialog appears if there are methods referring to this class (accessors methods for example), and offers a choice to open a (scoped) browser for this messages.
No accessors method will be changed or generated.
'
