pages
overview
	^HelpTopic
		title: 'Overview'
		contents: 
'
This chapter describes the steps needed to set up an environment and fill in the data
to execute refactoring operations.

This enables you to execute refactorings that aren''t provided by the System Browser, or combining a set of operations for a more complex refactoring. It also gives a hint on how to add refactoring support for your own tools.


These are the steps used for all of the examples:
- create a RBNamespace
- instantiate the refactoring operation
- execute it (primitiveExecute)
- open a changes browser to view and apply the changes
  applying the changes will actually execute the code transformations ''for real''.
'