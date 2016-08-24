pages
combineOperationsExample
	^HelpTopic
		title: 'Combining operations - Add class with instance variables'
		contents: 
'
As we saw, the RBAddClassRefactoring does not allow us to define any instance variables. Instead we can add a new class and then apply another refactoring, RBAddInstanceVariableRefactoring.
We just need to call them in the appropriate order and make sure that both operations operate on the same model - otherwise the instance variable refactoring would not know about the class it
should operate on.

  | model addClassRB addInstVarsRB browser |
    model := RBNamespace new.
    addClassRB := RBAddClassRefactoring
        model: model
        addClass: #SomeClass
        superclass: #Object
        subclasses: {}
        category: #Category.
    addClassRB primitiveExecute.
    addInstVarsRB := RBAddInstanceVariableRefactoring
        model: model
        variable: ''x''
        class: #SomeClass.
    addInstVarsRB primitiveExecute.
    browser := ChangesBrowser new.
    browser := ChangesBrowser changes: (model changes changes ).
    browser open

It is important to actually execute the first operation before creating the second one. The instantiation of the RBAddInstanceVariableRefactoring will query the environment the class #SomeClass and
init the reference to nil if it doesn''t yet exists.

The changes browser now includes two refactorings, you can select only the second one but this won''t work.
If you applied both, and want to undo that changes, you''ll need to call two times:
RBRefactoryChangeManager instance undoOperation.
'