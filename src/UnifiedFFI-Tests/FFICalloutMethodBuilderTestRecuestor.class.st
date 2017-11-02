"
I'm a callout for testing the method builder. 
Basically, I do not introspect any variable :P
"
Class {
	#name : #FFICalloutMethodBuilderTestRecuestor,
	#superclass : #FFICallout,
	#category : 'UnifiedFFI-Tests'
}

{ #category : #'argument loaders' }
FFICalloutMethodBuilderTestRecuestor >> loaderFromMethodArgsNamed: argName [
	(argName beginsWith: 'var')
		ifTrue: [ ^ 
			FFIMethodArgument new  
				argName: argName;
				index: (argName allButFirst: 3) asInteger ].
	^ nil
]
