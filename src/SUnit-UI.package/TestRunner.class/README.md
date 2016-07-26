<lint: #ignore rule: #classNotReferenced rational: 'this view is only accessed from menus'>

Although tests can be most easily be run from a browser, I provide the ability to: 
* select sets of test suites to run. I am designed to make it easy to execute groups of tests
* obtain a more detailed log of the results 


UI Description 
___________ 

My left-most pane lists all of the categories that contain test classes (i.e., subclasses of TestCase); when some of these categories are selected, the test classes that they contain appear in the pane to the right. Abstract classes are italicized, and the test class hierarchy is shown by indentation, so subclasses of ClassTestCase are indented more than subclasses of TestCase. 

The tests that did not pass (if any) are listed in the right-hand panes of the Test Runner; if you want to debug one, to see why it failed, just click on the name.
