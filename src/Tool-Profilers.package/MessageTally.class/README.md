My instances observe and report the amount of time spent in methods.

NOTE: a higher-level user interface (combining the MessageTally result tree with a method browser) is available from TimeProfileBrowser. Note that TimeProfileBrowser was not fancy with the different setting possibilities.

	TimeProfileBrowser spyOn:  [20 timesRepeat: 
			[Transcript show: 100 factorial printString]]
	

Strategies
-----------
MessageTally provides two different strategies available for profiling:

* spyOn: and friends use a high-priority Process to interrupt the block or process being spied on at periodic intervals. The interrupted call stack is then examined for caller information. See below for an example showing different settings

* tallySends: and friends use the interpreter simulator to run the block, recording every method call.

The two give you different results:

	* spyOn: gives you a view of where the time is being spent in your program, at least on a rough statistical level (assuming you've run the 	block for long enough and have a high enough poll rate). If you're trying to optimize your code, start here and optimize the methods where 	most of the time is being spent first.

	* tallySends: gives you accurate counts of how many times methods get called, and by exactly which route. If you're debugging, or trying to 	figure out if a given method is getting called too many times, this is your tool.

Q: How do you interpret MessageTally>>tallySends
A: The methods #tallySends and #spyOn: measure two very different quantities, but broken down in the same who-called-who format.  #spyOn: is approximate, but more indicative of real time spent, whereas #tallySends is exact and a precise record of how many times each method got executed.

Examples
----------

Here you can see all the processes computation time
	
		[1000 timesRepeat: [3.14159 printString. Processor yield]] fork.
		[1000 timesRepeat: [30 factorial. Processor yield]] fork.
		[1000 timesRepeat: [30 factorial. Processor yield]] fork.
		MessageTally spyAllOn: [ (Delay forMilliseconds: 100) wait] 


Settings
---------
You can change the printing format (that is, the whitespace and string compression) by using these instance methods: 
	maxClassNameSize:
	maxClassPlusSelectorSize:
	maxTabs:

You can change the default polling period (initially set to 1) by calling
	MessageTally defaultPollPeriod: numberOfMilliseconds


To understand the difference
----------------------------------
Here we see all the processes
	[1000 timesRepeat: [
		100 timesRepeat: [120 factorial].
		(Delay forMilliseconds: 10) wait
		]] forkAt: 45 named: '45'.
	MessageTally spyAllOn: [10000 timesRepeat: [1.23 printString]]
	
	
Here we only see the execution of the expression [10000 timesRepeat: [1.23 printString]
	[1000 timesRepeat: [
		100 timesRepeat: [120 factorial].
		(Delay forMilliseconds: 10) wait
		]] forkAt: 45 named: '45'.
	MessageTally spyOn: [10000 timesRepeat: [1.23 printString]]
	
Here we only check the exact message sends: this is not a pc-sampling approach
	[1000 timesRepeat: [
		100 timesRepeat: [120 factorial].
		(Delay forMilliseconds: 10) wait
		]] forkAt: 45 named: '45'.
	MessageTally tallySends: [10000 timesRepeat: [1.23 printString]]
	


