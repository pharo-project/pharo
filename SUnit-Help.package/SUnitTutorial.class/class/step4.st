pages
step4
	"This method was automatically generated. Edit it using:"
	"SUnitTutorial edit: #step4"
	^HelpTopic
		title: 'Step 4 - Using the TestRunner'
		contents: 
'Testing by evaluating an expression as we is not very convenient - so it is better to open the TestRunner - a user interface tool displaying all the tests in the system. You can start it either using the world menu or by evaluating 

    TestRunner open

in a workspace.

Scroll down until you find the class category ''MyApp-Tests-Model'' and select the test by clicking on the class PersonTest. It is possible to select more than one test/category. Now click on "Run selected". 

If you run the test in the test runner it will be marked red since it contains errors. The test runner will also show you the result of the test run:

     1 run, 0 passes, 0 skipped, 0 expected failures, 0 failures, 1 errors, 0 unexpected passes

If you click on the failed method in the lower right pane the debugger will open.!' readStream nextChunkText