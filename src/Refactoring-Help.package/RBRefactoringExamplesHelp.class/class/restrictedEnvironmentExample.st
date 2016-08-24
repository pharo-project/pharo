pages
restrictedEnvironmentExample
	^HelpTopic
		title: 'Scoping Refactoring'
		contents: 
'
A refactoring operation often changes existing code by first, search for a pattern, and then transform the matching code into the new form. But in some situations you don''t want to apply the change to all found matches.
For example, you want to rename a method in all implementors and all callers of your package. If this method name is a common message in other (system) classes as well, you don''t want to rename all places and for sure you don''t want to go through the ChangesBrowser and unselect all those matches by hand.

We can restrict the search space by creating our namespace from a restricted browser environment.
(More about restricted environments in the chapter RBBrowserEnvironments)

In this example we will apply the RBPrettyPrintCodeRefactoring to all classes in the Package ''Tests'', by first creating a RBBrowserEnvironment for packages and then create the RBNamespace with this environment:

 | env model prettyPrintRB browser |
    env := RBBrowserEnvironment new forPackageNames:{''Tests''}.
    model := RBNamespace onEnvironment: env.
    prettyPrintRB := RBPrettyPrintCodeRefactoring new model: model; yourself.
    prettyPrintRB primitiveExecute.
    browser := ChangesBrowser new.
    browser := ChangesBrowser changes: (model changes changes ).
    browser open

After applying this refactoring, all methods in all classes of the package ''Tests'' will be reformatted (pretty print).
'