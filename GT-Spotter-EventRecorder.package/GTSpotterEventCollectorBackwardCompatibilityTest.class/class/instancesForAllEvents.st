serializing
instancesForAllEvents
	"It instantiates all Spotter events and set all instance variables."
	^ GTSpotterRecorderEvent allSubclasses collect: [ :eachEventClass | | instance |
		instance := eachEventClass new.
		1 to: eachEventClass instSize do: [ :index |
			(instance instVarAt: index) ifNil: [ | variableName |
				variableName := eachEventClass instVarNameForIndex: index.
				(instance respondsTo: variableName) ifTrue: [ instance perform: variableName ] ].
			(instance instVarAt: index) ifNil: [ 
				instance instVarAt: index put: #value ] ].
		instance
	 ]