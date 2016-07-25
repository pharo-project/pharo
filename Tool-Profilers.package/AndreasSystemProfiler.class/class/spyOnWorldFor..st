spying
spyOnWorldFor: seconds
	"Run the system profiler for the specified number of seconds,
	 spying on the morphic world in which it was launched. 
	 Handy for running Workspace do-its."

	^self spyOn: [  | deadline |
		deadline := Time totalSeconds + seconds.
		[Time totalSeconds < deadline]
			whileTrue: [ World doOneCycle ]].