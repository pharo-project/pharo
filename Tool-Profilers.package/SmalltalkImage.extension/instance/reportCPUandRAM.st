*Tool-Profilers
reportCPUandRAM
	"Write several text files with useful analysis for profiling purposes.
	Overwrites any existing report.
	SmalltalkImage current reportCPUandRAM
	"	

	| stream |
	"VM statistics (Memory use and GC, mainly)"
	FileStream 
		forceNewFileNamed: 'Log-MemoryStats.txt'
		do: [:aStream | aStream nextPutAll: self vm statisticsReport ].
	
	"Process list"
	stream := FileStream 
				forceNewFileNamed: 'Log-ProcessList.txt' 
				do: [ :aStream |  self tools processBrowser new processNameList 
								do: [ :each | aStream nextPutAll: each; cr ]].

"Fork all these, so they run in sequence, as the system is back running"
[
	"Process taking most CPU"
	stream := FileStream forceNewFileNamed: 'Log-ThePig.txt'.
	self tools processBrowser dumpPigStackOn: stream andClose: true.
	
	"Tally of all processes"
	stream := FileStream forceNewFileNamed: 'Log-FullTally.txt'.
	[ | tally |
		tally := MessageTally new.
		tally reportOtherProcesses: true.
		tally spyAllEvery: 1 on: [ (Delay forMilliseconds: 1000) wait ].
		tally report: stream ] ensure: [ stream close ].

	"Memory Analysis"
	stream := FileStream forceNewFileNamed: 'Log-MemoryAnalysis.txt'.
	[ SpaceTally new printSpaceAnalysis: 1 on: stream ]
		ensure: [ stream close ]

] forkNamed: 'CPU usage analysis'