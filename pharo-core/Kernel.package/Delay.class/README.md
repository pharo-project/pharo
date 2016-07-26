I am the main way that a process may pause for some amount of time.  The simplest usage is like this:

	(Delay forSeconds: 5) wait.   "or..."
	5 seconds asDelay wait.

An instance of Delay responds to the message 'wait' by suspending the caller's process for a certain amount of time. The duration of the pause is specified when the Delay is created with the message forMilliseconds: or forSeconds:. A Delay can be used again when the current wait has finished. For example, a clock process might repeatedly wait on a one-second Delay.  Delays work across  clock roll-overs.

The maximum possible delay depends on which DelayScheduler is used:
  * DelayMillisecondScheduler uses a 32-bit value that rolls over about every six days, or SmallInteger maxVal // 2 milliseconds.
  * DelayMicrosecondScheduler is 64-bit and rolls over every 50,000 years. 

A delay in progress when an image snapshot is saved and resumed when the snapshot is re-started. 
