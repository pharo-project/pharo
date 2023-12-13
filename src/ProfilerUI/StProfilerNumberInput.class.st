"
I am a number input text to introduce percentage threshold to filter / expand / collapse in the profiler results tree
"
Class {
	#name : 'StProfilerNumberInput',
	#superclass : 'SpNumberInputFieldPresenter',
	#category : 'ProfilerUI-View',
	#package : 'ProfilerUI',
	#tag : 'View'
}

{ #category : 'initialization' }
StProfilerNumberInput >> initialize [

	super initialize.
	self
		beFloat;
		digits: 1;
		climbRate: 0.5;
		minimum: 0;
		maximum: 100
]

{ #category : 'initialization' }
StProfilerNumberInput >> initializePresenters [

	self eventHandler
		whenFocusLostDo: [
			"if text is not a number, set to zero"
			self text asInteger ifNil: [ self number: 0 ] ];
		whenFocusReceivedDo: [ self selectAll ];
		whenMouseDownDo: [ :evt | self selectAll ]
]
