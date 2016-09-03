I am special environment to manage test execution. I address three problems:

1) Tests should never hang. They should be executed with time limit. 
I give them 500 milliseconds by default. It could be overriden by TestCase method #defaultTimeLimit.
Or it could be specified directly in test method by 
	self timeLimit: 10 seconds
It could be changed at any time of test execution.

To implement this logic I maintain special watch dog process which control execution time of tests. It is single for all test suite.

2) When test completes I terminate all running processes which were forked during execution. 

3) I manage all failures from forked processes by preventing spawning debuggers. I mark such tests as failed by signalling TestForkedFailedProcess error.
When failure is signelled from forked process I suspend it and collect them together inside failedProcesses dictionary.
TestForkedFailedProcess signal is resumable to allow debug suspended failures. When you debug test and saw this problem you can press Proceed to debug actual failures.

I am installed when test is running (or test suite) by
	CurrentExecutionEnvironment runTestCase: aTestCase
	
Internal Representation and Key Implementation Points.

    Instance Variables
	failedProcesses:		<Dictionary of<Process->Error>>
	forkedProcesses:		<OrderedCollection of<Process>>
	maxTimeForTest:		<Duration>
	testCase:		<TestCase>
	watchDogProcess:		<Process>
	watchDogSemaphore:		<Semaphore>