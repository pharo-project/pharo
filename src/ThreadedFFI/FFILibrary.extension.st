Extension { #name : #FFILibrary }

{ #category : #'*ThreadedFFI' }
FFILibrary >> ffiCall: fnSpec [
	<ffiCalloutTranslator>

	^ (self ffiCalloutIn: thisContext sender)
		convention: self ffiCallingConvention;
		options: self ffiLibrary options;
		function: fnSpec module: self
]

{ #category : #'*ThreadedFFI' }
FFILibrary >> ffiCall: fnSpec options: callOptions [
	<ffiCalloutTranslator>
	
	^ (self ffiCalloutIn: thisContext sender)
		convention: self ffiCallingConvention;
		options: (self ffiLibrary options), callOptions;
		function: fnSpec module: self
]
