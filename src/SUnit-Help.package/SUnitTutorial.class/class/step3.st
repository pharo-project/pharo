pages
step3
	"This method was automatically generated. Edit it using:"
	"SUnitTutorial edit: #step3"
	^HelpTopic
		title: 'Step 3 - Running our first test method'
		contents: 
'To run our test case method we can evaluate the following code snippet in a workspace:

	PersonTest run: #testInstanceCreation

If you print the result then the following will appear:

      1 run, 0 passes, 0 skipped, 0 expected failures, 0 failures, 1 errors, 0 unexpected passes

This tells us that one test has been run and one error occured while testing. Currently we dont have all messages implemented - so the test has to fail. 

If you want to debug the test to see whats happened you can evaluate:

     PersonTest debug: #testInstanceCreation



 
	!' readStream nextChunkText