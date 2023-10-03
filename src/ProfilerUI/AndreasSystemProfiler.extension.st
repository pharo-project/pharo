Extension { #name : 'AndreasSystemProfiler' }

{ #category : '*ProfilerUI' }
AndreasSystemProfiler >> blockCodeFrom: someCode [
	^ '[' , someCode , '] value'
]

{ #category : '*ProfilerUI' }
AndreasSystemProfiler >> doItSelectorsPath [
	^ #( 'nil>>nil'
		  'FullBlockClosure>>newProcess'
	     'ProfilerModel>>profileBlock:'
		  'FullBlockClosure>>ensure:'
	     'ProfilerModel>>DoIt'
		  'ProfilerModel>>DoIt' )
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
