*GT-Debugger-Model
registerDebugger: anObject withRank: anInteger
	|debuggers|
	
	debuggers  := tools at: #debuggers ifAbsent: [
		tools at: #debuggers put: OrderedCollection new ]. 
	(debuggers anySatisfy: [:each| each key == anObject] ) ifFalse: [ 
		debuggers add: anObject -> anInteger ].
	WorldState defaultWorldMenu