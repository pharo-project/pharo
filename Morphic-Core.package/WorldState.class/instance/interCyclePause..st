update cycle
interCyclePause: milliSecs
	"delay enough that the previous cycle plus the amount of delay will equal milliSecs.  If the cycle is already expensive, then no delay occurs.  However, if the system is idly waiting for interaction from the user, the method will delay for a proportionally long time and cause the overall CPU usage of Squeak to be low.
	If self serverMode returns true then, always do a complete delay of 50ms, independant of my argument. This prevents the freezing problem described in Mantis #6581"

	| wait wait2 |
	wait := self serverMode
    	ifTrue: [ 50 ]
    	ifFalse: 
		[	wait2 := (lastCycleTime notNil and: [CanSurrenderToOS ~~ false]) 
        		ifFalse: [ 0 ]
        		ifTrue: [ lastCycleTime + milliSecs - Time millisecondClockValue ].
 
      		self flag: 'Issue 14754 - wait2>millisecs is only True for clock rollover. Remove it once DelayScheduler based on microsecondClock - Ben Coman 19.01.2015'.
      		wait2 > milliSecs 
        		ifTrue: [ 0 ]
        		ifFalse: [ wait2 ]. 
		].
		
	wait > 0 ifTrue: [	(Delay forMilliseconds: wait) wait ].

	lastCycleTime := Time millisecondClockValue.
	CanSurrenderToOS := true.