"
I store meta-data for a package. I'm the common superclass of all package Manifest.
"
Class {
	#name : #PackageManifest,
	#superclass : #Object,
	#category : #'Kernel-Manifest'
}

{ #category : #'accessing comment' }
PackageManifest class >> classCommentBlank [

	^'No description for this package available. Please add a description for this package here'
]

{ #category : #'meta-data' }
PackageManifest class >> description [
	^ 'No description for this package manifest available. Please add a description for this package here.'
]

{ #category : #'meta-data - dependency analyser' }
PackageManifest class >> ignoredDependencies [
	^ #(#'System-Settings-Core')
]

{ #category : #deprecation }
PackageManifest class >> isDeprecated [
	^ false
]

{ #category : #testing }
PackageManifest class >> isManifest [
	^ (self = PackageManifest) not
]

{ #category : #'System-Support' }
PackageManifest class >> isUsed [
	^ true
]

{ #category : #'meta-data - dependency analyser' }
PackageManifest class >> manuallyResolvedDependencies [
	^ #()
]

{ #category : #unloading }
PackageManifest class >> postUnloadAction [

	"override to return block that will be executed after the package unloading. Keep in mind that all package classes are already removed from the system including the manifest class"
	
	^ [ "do nothing" ]
]

{ #category : #unloading }
PackageManifest class >> preUnload [

	"override this method to specify clean-up actions that should be done before package unloading."
]

{ #category : #'code-critics' }
PackageManifest class >> rejectClasses [
	^ #()
]

{ #category : #'code-critics' }
PackageManifest class >> rejectRules [
	^ #()
]
