"
I'm a variant of FFICalloutMethodBuilder  that adds a call to  #signalIfNotReady  before the actual ffi call.
"
Class {
	#name : #LGitSafeFFICalloutMethodBuilder,
	#superclass : #FFICalloutMethodBuilder,
	#category : #'LibGit-Core-FFI-Handling'
}

{ #category : #private }
LGitSafeFFICalloutMethodBuilder >> generateFFICallout: builder spec: functionSpec [
	"Before the actual ffi call generate a check to see if the object is ready."
	builder 
		pushReceiver;
		send: #signalIfNotReady.
	^ super generateFFICallout: builder spec: functionSpec
]
