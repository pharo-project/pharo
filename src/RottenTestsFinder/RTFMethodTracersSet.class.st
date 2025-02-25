"
I am a set of method tracers.

I provide an API to avoid creating a mess with tracers not uninstalled after used.
"
Class {
	#name : 'RTFMethodTracersSet',
	#superclass : 'Object',
	#instVars : [
		'tracers'
	],
	#category : 'RottenTestsFinder-Tracers',
	#package : 'RottenTestsFinder',
	#tag : 'Tracers'
}

{ #category : 'instance creation' }
RTFMethodTracersSet class >> onCompiledMethods: aCollectionOfCompiledMethods [
	^ self new
		tracers: (aCollectionOfCompiledMethods collect: [ :compiledMethod | RTFMethodTracer on: compiledMethod ]);
		yourself
]

{ #category : 'instance creation' }
RTFMethodTracersSet class >> tracers: aCollectionOfRTFMethodTracers [
	^ self new
		tracers: aCollectionOfRTFMethodTracers;
		yourself
]

{ #category : 'testing' }
RTFMethodTracersSet >> anyTracerHasRun [
	^ self tracers anySatisfy: [ :tracer | tracer hasRun ]
]

{ #category : 'accessing' }
RTFMethodTracersSet >> compiledMethods [
	^ self tracers collect: [ :tracer | tracer method ]
]

{ #category : 'accessing' }
RTFMethodTracersSet >> compiledMethodsForTracersRun [
	^ self tracersRun collect: [ :tracer | tracer method ]
]

{ #category : 'actions' }
RTFMethodTracersSet >> installAll [
	self tracers do: [ :tracer | tracer install ]
]

{ #category : 'testing' }
RTFMethodTracersSet >> noTracerHasRun [
	^ self tracers noneSatisfy: [ :tracer | tracer hasRun ]
]

{ #category : 'accessing' }
RTFMethodTracersSet >> selectors [
	^ self compiledMethods collect: #selector
]

{ #category : 'accessing' }
RTFMethodTracersSet >> tracers [
	^ tracers
]

{ #category : 'accessing' }
RTFMethodTracersSet >> tracers: anObject [
	tracers := anObject
]

{ #category : 'accessing' }
RTFMethodTracersSet >> tracersRun [
	^ self tracers select: [ :tracer | tracer hasRun ]
]

{ #category : 'actions' }
RTFMethodTracersSet >> uninstallAll [
	self tracers do: [ :tracer | tracer uninstall ]
]

{ #category : 'actions' }
RTFMethodTracersSet >> unmarkAll [
	self tracers do: [ :tracer | tracer unmark ]
]

{ #category : 'actions' }
RTFMethodTracersSet >> whileTracersAreInstalledDo: aBlock [
	[
		self installAll.
		aBlock cull: self
	] ensure: [ self uninstallAll ]
]
