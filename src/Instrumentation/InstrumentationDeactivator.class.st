Class {
	#name : 'InstrumentationDeactivator',
	#superclass : 'InstrumentationEnsurer',
	#category : 'Instrumentation',
	#package : 'Instrumentation'
}

{ #category : 'accessing' }
InstrumentationDeactivator >> value [

	<noInstrumentation>
	thisProcess shiftLevelDown
]
