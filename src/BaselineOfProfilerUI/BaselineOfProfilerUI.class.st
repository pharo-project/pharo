Class {
	#name : 'BaselineOfProfilerUI',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfProfilerUI',
	#package : 'BaselineOfProfilerUI'
}

{ #category : 'baselines' }
BaselineOfProfilerUI >> baseline: spec [

	<baseline>
	spec for: #common do: [ spec package: 'ProfilerUI' ]
]
