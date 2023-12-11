Extension { #name : 'AndreasSystemProfiler' }

{ #category : '*ProfilerUI' }
AndreasSystemProfiler >> doItBlockSelectorsPath [
	^ self doItSelectorsPathCommonPart, #( 
	     'UndefinedObject>>DoIt' )
]

{ #category : '*ProfilerUI' }
AndreasSystemProfiler >> doItCodeSelectorsPath [
	^ self doItSelectorsPathCommonPart, #( 
	     'ProfilerModel>>DoIt' )
]

{ #category : '*ProfilerUI' }
AndreasSystemProfiler >> doItCommandSelectorsPath [
	^ self doItSelectorsPathCommonPart, #( 
		  'SpCodeProfileItCommand>>execute'
		  'CompiledMethod>>valueWithReceiver:arguments:'
	     'UndefinedObject>>DoIt' )
]

{ #category : '*ProfilerUI' }
AndreasSystemProfiler >> doItSelectorsPathCommonPart [
	^ #( 'nil>>nil'
		  'FullBlockClosure>>newProcess'
	     'ProfilerModel>>forkProfileBlock:'
		  'FullBlockClosure>>ensure:'
	     'ProfilerModel>>forkProfileBlock:'
	     'AndreasSystemProfiler>>runBlock:' )
]

{ #category : '*ProfilerUI' }
AndreasSystemProfiler >> doItSelectorsPaths [

	^ Array
		  with: self doItCodeSelectorsPath
		  with: self doItBlockSelectorsPath
		  with: self doItCommandSelectorsPath
]

{ #category : '*ProfilerUI' }
AndreasSystemProfiler >> runBlock: aBlock [
	"Compatibility with TimeProfiler"

	^ aBlock value
]

{ #category : '*ProfilerUI' }
AndreasSystemProfiler >> tallyRoot [
	^ tallyRoot
]

{ #category : '*ProfilerUI' }
AndreasSystemProfiler >> totalTally [
	^ totalTally
]

{ #category : '*ProfilerUI' }
AndreasSystemProfiler >> totalTime [
	^ totalTime
]
