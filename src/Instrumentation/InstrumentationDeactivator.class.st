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

	<methodProxyCannotWrap>
	thisProcess shiftLevelDown
]
