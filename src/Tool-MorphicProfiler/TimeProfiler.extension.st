Extension { #name : 'TimeProfiler' }

{ #category : '*Tool-MorphicProfiler' }
TimeProfiler class >> menuProfilingOn: aBuilder [
	<worldMenu>

	(aBuilder item: #'Start profiling all Processes')
		parent: #Profiling;
		help: 'Profile all Pharo processes.';
		order: 10;
		action: [ self currentWorld worldState startMessageTally ].
	(aBuilder item: #'Start profiling UI ')
		parent: #Profiling;
		help: 'Profile the UI process of Pharo.';
		order: 20;
		action: [ self currentWorld worldState startThenBrowseMessageTally ]
]
