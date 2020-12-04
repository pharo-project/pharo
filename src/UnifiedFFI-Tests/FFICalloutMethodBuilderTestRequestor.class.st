"
I'm a callout for testing the method builder. 
Basically, I do not introspect any variable :P
"
Class {
	#name : #FFICalloutMethodBuilderTestRequestor,
	#superclass : #FFICallout,
	#category : #'UnifiedFFI-Tests-Test-Data'
}

{ #category : #'argument loaders' }
FFICalloutMethodBuilderTestRequestor >> loaderFromMethodArgsNamed: argName [
	(argName beginsWith: 'var')
		ifTrue: [ ^ 
			FFIMethodArgument new  
				argName: argName;
				index: (argName allButFirst: 3) asInteger ].
	^ nil
]
