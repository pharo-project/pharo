pages
refactoringOptions
	^HelpTopic
		title: 'Refactoring Options'
		contents: 
'
Some refactoring operations may require additional informations for performing the transformation. For example a ''move method '' refactoring, moving a method from one class to another may add an additional argument if the prior method had some ''self sends''. Some of the information are given by instantiating the refactoring and some information can be computed by the
operation itself. For other cases the refactoring may actually break code or create broken code. To make this operation still work the programmer or user of the refactoring engine
could provide the needed information. 

For this, the engine contains a set of ''options'' that can be set by the tool using the framework, to register callback functions used to aquire the information from the user.

The options that can be used are:

#implementorToInline - select one of a list of method names

#methodName - ask for a method name

#selfArgumentName - argument name to use for replacing self sends

#selectVariableToMoveTo - select one of a list of variable names

#variableTypes - select or provide a class

#extractAssignment - should the code extraction include the variable assignment

#inlineExpression - I don''t know

#alreadyDefined - Should it override  methods defined in the hierarchy.

#useExistingMethod - Should it use existing (equivalent) method

#openBrowser - call to open system browser

A tool now can register a callback like

refactoring setOption:#name_of_an_option toUse:[:a :b: ... a block with needed arguments]

for example, Nautilus sets the option
#implementorToInline to a method showing a dialog with a list to choose one of the provided selector names.

In the following example we show how to set the needed options manually. The RBMoveMethodRefactoring will ask us three questions
- selfArgumentName
- variableTypes
- methodName

for all of this options we set a simple block that just returns the information needed for this example task. In a real world tool, we
would need some interactive tool to let the user make a choice.
This RBMoveMethodRefactoring will move the implementation from TestResult class>>#historyFor: to its argument of type TestCase

    | model rbMoveMethod browser |
    model := RBNamespace onEnvironment: RBBrowserEnvironment new.
    rbMoveMethod := RBMoveMethodRefactoring
        model: model
        selector: #historyFor:
        class: TestResult class
        variable: ''aTestCaseClass''.
    rbMoveMethod setOption: #selfArgumentName toUse: [ :ref | ''aResultClass'' ].
    rbMoveMethod
        setOption: #variableTypes
        toUse: [ :ref :types :selected | {(model classNamed: #TestCase)} ].
    rbMoveMethod
        setOption: #methodName
        toUse: [ :ref :name | RBMethodName selector: ''asHistoryFor:'' arguments: {''aTestResult''} ].
    rbMoveMethod primitiveExecute.
    browser := ChangesBrowser changes: model changes changes.
    browser open

The result of this operation is, the method #historyFor: is moved to the class TestCase and the former implementation is replaced by
  aTestCase asHistoryFor: self
as the former implementation had a call to self (self newTestDictionary) we need to add self as an argument for the new method.
The refactoring operation queries this argument name by calling the registered block for the option ''selfArgumentName'', as the refactoring can not guess the type
of the class we want to move the method, it will ask us by calling ''variableTypes'' and finally the new method name and arguments are provided by calling the block for option
''methodName''.
'