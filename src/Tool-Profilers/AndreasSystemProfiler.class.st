"
AndreasSystemProfiler uses sub-msec VM supported PC sampling.

	In Memory of Andreas Raab.  Author, Friend, Colleague. 	http://forum.world.st/In-Memory-of-Andreas-Raab-td4663424.html
	Released by Ron, Julie and David

Example:
AndreasSystemProfiler spyOn: [ 10000 timesRepeat: [ 3.14159 printString ] ]

-=-=-=-=-=-=-=
Apparently, the time taken to run the provided block is as twice as long as run without the profiler.

-=-=-=-=-=-=-=
Both AndreasSystemProfiler and MessageTally are periodic sampling profilers.  The essential difference between AndreasSystemProfiler and MessageTally is in how the current method is sampled.

MessageTally is driven from a high-priority process in a loop waiting on a delay.  When the delay fires the lower-priority process being profiled is interrupted, its stack is walked to determine the methods along the call chain, and that data is recorded.  But since the sampling occurs when the high-priority process preempts the lower-priority process, a sample is only taken at a preemption point.  In particular, primitives are *not* profiled because they are not suspension points.  A process can only be suspended on method activation (a non-primitive method activation, or primitive failure) or on backward branch.  The cost of primitives is charged to a caller and is inferred by subtracting the cost of children of the caller from the caller itself (subtracting the number of samples in children of the caller form the number of samples in the caller itself).  

Another problem is that using the clock that underlies Delay, which is typically the clock used by processes being profiled, causes sampling errors due to the sampling and sampled processes cohering.  Delays are limited in resolution (at best 1 millisecond) so if the profiled process waits on a delay it'll fire immediately after the profiling process (because the profiling process is at higher priority) and so the sampling process may only ever see the sampled process in a wait state.

If MessageTally is used to profile multiple processes then a third problem is that if a primitive causes a process switch then its cost will end up being charged to the process switched-to, not switched from.  This is again because sampling can only occur after a primitive has completed (successfully or not).

AndreasSystemProfiler is driven from a high-priority process in a loop waiting on a Semaphore known to the VM.  The profiling process uses a primitive to schedule a sample some number of ticks of the VM's high-performance clock in the future.  When the time is reached the VM samples the current method and the current process, *before any process preemption takes place*, and independently of the standard clock, and signals the semaphore.  The profiling process then collects the method,process pair via primitives.  So AndreasSystemProfiler provides much more accurate results.

That said there are still limitations with primitives and Cog.  Currently Cog only samples ""interpreter"" primitives.  Those primitives it implements in machine code (integer and float arithmetic, closure evaluation, at:, identityHash) are not sampled and won't show up; they will be charged to the calling method.  This is fixable, since Cog actually compiles the sampling direct into interpreter primitive invocation when profiling is in effect and not at other times, but sampling could be a significant cost in these simple and performance-critical primitives.
"
Class {
	#name : #AndreasSystemProfiler,
	#superclass : #Object,
	#instVars : [
		'semaphore',
		'ticks',
		'profilerProcess',
		'tallyRoot',
		'vmStats',
		'totalTally',
		'totalTime',
		'startTime',
		'ticksPerMSec',
		'totalTicks'
	],
	#category : #'Tool-Profilers-System'
}

{ #category : #LICENSE }
AndreasSystemProfiler class >> LICENSE [
	^'Project Squeak

	In Memory of Andreas Raab.  Author, Friend, Colleague. 	http://forum.world.st/In-Memory-of-Andreas-Raab-td4663424.html
	Copyright (c) 2005-2013, 3D Immersive Collaboration Consulting, LLC., All Rights Reserved

	Redistributions in source code form must reproduce the above
	copyright and this condition.

Licensed under MIT License (MIT)
Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'
]

{ #category : #spying }
AndreasSystemProfiler class >> default [
	^self new
]

{ #category : #examples }
AndreasSystemProfiler class >> exampleSpyForSeconds [

	self spyFor: 5
]

{ #category : #examples }
AndreasSystemProfiler class >> exampleSpyOnBlock [

	self spyOn: [ 10000 timesRepeat: [ 3.14159 printString ] ]
]

{ #category : #examples }
AndreasSystemProfiler class >> exampleSpyOnWorld [

	self spyOnWorldFor: 5
]

{ #category : #spying }
AndreasSystemProfiler class >> spyFor: seconds [
	"Run the system profiler for the specified number of seconds"

	^self spyOn: [ (Delay forSeconds: seconds) wait ].
]

{ #category : #spying }
AndreasSystemProfiler class >> spyOn: aBlock [
	"The main method for profiling and showing the report "

	| profiler |
	profiler := self new.
	^ [ profiler spyOn: aBlock ]
		ensure: [ profiler doReport ]
]

{ #category : #spying }
AndreasSystemProfiler class >> spyOnWorldFor: seconds [
	"Run the system profiler for the specified number of seconds,
	 spying on the morphic world in which it was launched. 
	 Handy for running Workspace do-its."

	^self spyOn: [  | deadline |
		deadline := Time totalSeconds + seconds.
		[Time totalSeconds < deadline]
			whileTrue: [ self currentWorld doOneCycle ]].
]

{ #category : #reporting }
AndreasSystemProfiler >> doReport [
	"Report the results of this profiler run"
	UIManager default edit: self report label: 'Spy Results'.
]

{ #category : #reporting }
AndreasSystemProfiler >> report [
	"Answer a report, with cutoff percentage of each element of the tree"
	^String streamContents: [ :s | self report: s ]
]

{ #category : #reporting }
AndreasSystemProfiler >> report: strm [ 
	"Print a report, with cutoff percentage of each element of the tree 
	(leaves, roots, tree)=2, on the stream, strm."

	self report: strm cutoff: 1
]

{ #category : #reporting }
AndreasSystemProfiler >> report: strm cutoff: threshold [ 
	tallyRoot ifNil: [ strm nextPutAll: 'The profiler has not been run'. ^ self ].
	
	tallyRoot tally isZero
		ifTrue: [ strm nextPutAll: ' - no tallies obtained' ] 
		ifFalse: [
			strm nextPutAll: 'Reporting - ', totalTally asStringWithCommas,' tallies, ',
							 totalTime asStringWithCommas, ' msec.'; cr; cr.
			tallyRoot fullPrintOn: strm tallyExact: false orThreshold: threshold time: totalTime.
	].

	totalTime isZero ifFalse: [
		self reportGCStatsOn: strm.
		self reportProcessStatsOn: strm.
	].
]

{ #category : #reporting }
AndreasSystemProfiler >> reportGCStatsOn: str [
	| oldSpaceEnd youngSpaceEnd memoryEnd fullGCs fullGCTime incrGCs incrGCTime tenureCount upTime rootOverflows |
	upTime := totalTime.
	oldSpaceEnd			:= vmStats at: 1.
	youngSpaceEnd		:= vmStats at: 2.
	memoryEnd			:= vmStats at: 3.
	fullGCs				:= vmStats at: 7.
	fullGCTime			:= vmStats at: 8.
	incrGCs				:= vmStats at: 9.
	incrGCTime			:= vmStats at: 10.
	tenureCount			:= vmStats at: 11.
	rootOverflows		:= vmStats at: 22.

	str cr.
	str	nextPutAll: '**Memory**'; cr.
	str	nextPutAll:	'	old			';
		nextPutAll: oldSpaceEnd asStringWithCommasSigned; nextPutAll: ' bytes'; cr.
	str	nextPutAll: '	young		';
		nextPutAll: (youngSpaceEnd - oldSpaceEnd) asStringWithCommasSigned; nextPutAll: ' bytes'; cr.
	str	nextPutAll: '	used		';
		nextPutAll: youngSpaceEnd asStringWithCommasSigned; nextPutAll: ' bytes'; cr.
	str	nextPutAll: '	free		';
		nextPutAll: (memoryEnd - youngSpaceEnd) asStringWithCommasSigned; nextPutAll: ' bytes'; cr.

	str cr.
	str	nextPutAll: '**GCs**'; cr.
	str	nextPutAll: '	full			';
		print: fullGCs; nextPutAll: ' totalling '; nextPutAll: fullGCTime asStringWithCommas; nextPutAll: 'ms (';
		print: ((fullGCTime / upTime * 100) roundTo: 1.0);
		nextPutAll: '% uptime)'.
	fullGCs = 0 ifFalse:
		[str	nextPutAll: ', avg '; print: ((fullGCTime / fullGCs) roundTo: 1.0); nextPutAll: 'ms'].
	str	cr.
	str	nextPutAll: '	incr		';
		print: incrGCs; nextPutAll: ' totalling '; nextPutAll: incrGCTime asStringWithCommas; nextPutAll: 'ms (';
		print: ((incrGCTime / upTime * 100) roundTo: 1.0);
		nextPutAll: '% uptime)'.
	incrGCs = 0 ifFalse:
		[str nextPutAll:', avg '; print: ((incrGCTime / incrGCs) roundTo: 1.0); nextPutAll: 'ms'].
	str cr.
	str	nextPutAll: '	tenures		';
		nextPutAll: tenureCount asStringWithCommas.
	tenureCount = 0 ifFalse:
		[str nextPutAll: ' (avg '; print: (incrGCs / tenureCount) asInteger; nextPutAll: ' GCs/tenure)'].
	str	cr.
	str	nextPutAll: '	root table	';
		nextPutAll: rootOverflows asStringWithCommas; nextPutAll:' overflows'.
	str cr.

]

{ #category : #reporting }
AndreasSystemProfiler >> reportProcessStatsOn: str [
	| totalSwitches pageOverflows pageDivorces actualSwitches |
	vmStats size >= 61 ifFalse: [ ^ self ]. "don't try this on the closure VM"
	totalSwitches := vmStats at: 56.
	actualSwitches := totalSwitches - (2*totalTally). "exclude to/from profiler"
	pageOverflows := vmStats at: 60.
	pageDivorces := vmStats at: 61.

	str cr.
	str nextPutAll: '**Processes**'; cr.
	str tab; nextPutAll: 'Total process switches: ', totalSwitches printString; cr.
	str tab; nextPutAll: 'Without Profiler: ', actualSwitches printString; cr.
	str tab; nextPutAll: 'Stack page overflows: ', pageOverflows printString; cr.
	str tab; nextPutAll: 'Stack page divorces: ', pageDivorces printString; cr.

]

{ #category : #profiling }
AndreasSystemProfiler >> runProfilerProcess [
	"Run the profiler process"
	| process tallyStart tallyTicks tallyStop method leaf |
	tallyRoot := QSystemTally new class: nil method: nil.
	totalTally := 0.
	Smalltalk profileSemaphore: semaphore.
	totalTicks := 0.
	tallyStart := tallyStop := Smalltalk highResClock.
	[ true ] whileTrue: [
		tallyStart := tallyStop.
		tallyStart := Smalltalk highResClock.
		Smalltalk profileStart: ticks. "run for n ticks"
		semaphore wait.
		tallyStop := Smalltalk highResClock.
		tallyTicks := tallyStop - tallyStart.
		totalTicks := totalTicks + tallyTicks.
		process := Smalltalk profileSample.
		method := Smalltalk profilePrimitive.
		totalTally := totalTally + 1.
		process ifNotNil:[
			leaf := tallyRoot tally: (process suspendedContext ifNil: [ thisContext ] ) by: tallyTicks.
			method ifNotNil: [ leaf tallyMethod: method by: tallyTicks ].
		].
	].

]

{ #category : #profiling }
AndreasSystemProfiler >> spyOn: aBlock [
	"Profile system activity during execution of aBlock. 
	The argument is the desired samples per *milli* second.
	Mostly for polymorphism with MessageTally."
	self startProfiling.
	aBlock ensure: [ self stopProfiling ]

]

{ #category : #profiling }
AndreasSystemProfiler >> startProfiling [
	"Start the profiler process taking samplesPerMsec samples per *milli* second"
	| t0 |
	semaphore := Semaphore new.
	"Run a 100 msec busy loop to figure out the ticks per msec"
	t0 := Time millisecondClockValue + 2.
	[Time millisecondClockValue >= t0] whileFalse.
	ticksPerMSec := Smalltalk highResClock.
	[Time millisecondClockValue >= (t0 + 100)] whileFalse.
	ticksPerMSec := (Smalltalk highResClock - ticksPerMSec) 
			// (Time millisecondClockValue - t0).
	"Try to get 10 samples per msec"
	ticks := ticksPerMSec // 10.
	startTime := Time millisecondClockValue.
	vmStats := Smalltalk vm getParameters.
	profilerProcess := [ self runProfilerProcess ] forkAt: Processor timingPriority - 1.

]

{ #category : #profiling }
AndreasSystemProfiler >> stopProfiling [
	"Stop the profiler process"
	Smalltalk profileSemaphore: nil.
	Smalltalk profileStart: 0. "<- profile stops now"
	totalTime := Time millisecondClockValue - startTime.
	Smalltalk vm getParameters keysAndValuesDo: [ :idx :value | 
		value isInteger ifTrue: [ vmStats at: idx put: (value - ((vmStats at: idx) ifNil: [ 0 ])) ].
	].
	profilerProcess ifNotNil: [
		profilerProcess suspend.
		profilerProcess := nil.
	].

]
