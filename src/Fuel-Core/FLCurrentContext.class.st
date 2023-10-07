"
I am a dynamic variable that holds the current context.

To obtain the current configuration, uses should use the TFLConfigurable trait and send #context to themselves.
"
Class {
	#name : 'FLCurrentContext',
	#superclass : 'DynamicVariable',
	#category : 'Fuel-Core-Configuration',
	#package : 'Fuel-Core',
	#tag : 'Configuration'
}

{ #category : 'accessing' }
FLCurrentContext >> value: aContext during: aBlock [
	^ [ super
		value: aContext
		during: aBlock ] ensure: [
			aContext finalizeOperations ]
]
