Class {
	#name : #InstrumentationDeactivator,
	#superclass : #InstrumentationEnsurer,
	#pools : [
		'AnObsoleteMpMethodProxyPool'
	],
	#category : #Instrumentation
}

{ #category : #accessing }
InstrumentationDeactivator >> value [

	<noInstrumentation>
	thisProcess shiftLevelDown
]
