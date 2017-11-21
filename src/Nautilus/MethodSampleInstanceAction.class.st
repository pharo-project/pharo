"
An examplar method is tagged with <examplar>. 
It returns simply an instance. I can be used in tests.

An Example method may not return an instance and open something. 

To bridge the gap between the two, Nautilus offers to inspect instance returned by examplar tagged method.
"
Class {
	#name : #MethodSampleInstanceAction,
	#superclass : #AbstractMethodIconAction,
	#category : #Nautilus-Widgets
}

{ #category : #order }
MethodSampleInstanceAction >> actionOrder [
	"Return the priority of this action"
	
	^0
]

{ #category : #order }
MethodSampleInstanceAction >> isActionHandled [

	^ method methodClass isMeta
		and: [ method numArgs isZero and: [ method pragmas anySatisfy: [ :each | each keyword = #sampleInstance ] ] ]
]

{ #category : #order }
MethodSampleInstanceAction >> privateActionIcon [

	 ^IconicButton new 
			target: self browser;
			actionSelector: #runExamplarMethod:;
		 	arguments: { method };
			labelGraphic: (self iconNamed: #smallDoItIcon) ;
			color: Color transparent;
			extent: self iconExtent;
			helpText: 'Get the class sample (predefined instance) in a inspector to tweak it and have fun with it';
			borderWidth: 0 
]
