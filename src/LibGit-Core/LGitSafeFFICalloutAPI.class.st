"
I'm a variant of FFICalloutAPI that uses a different builder to generate methods with an extra check to see if the object is 'ready' or not.
"
Class {
	#name : #LGitSafeFFICalloutAPI,
	#superclass : #FFICalloutAPI,
	#category : #'LibGit-Core-FFI-Handling'
}

{ #category : #action }
LGitSafeFFICalloutAPI >> newBuilder [
	^ LGitSafeFFICalloutMethodBuilder calloutAPI: self
]
