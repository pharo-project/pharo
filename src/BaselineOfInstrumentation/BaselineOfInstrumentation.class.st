Class {
	#name : 'BaselineOfInstrumentation',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfInstrumentation',
	#package : 'BaselineOfInstrumentation'
}

{ #category : 'baselines' }
BaselineOfInstrumentation >> baseline: spec [

	<baseline>
	spec for: #common do: [ spec package: #Instrumentation ]
]
