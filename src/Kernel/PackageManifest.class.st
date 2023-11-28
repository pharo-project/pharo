"
I store meta-data for a package. I'm the common superclass of all package Manifest.
"
Class {
	#name : 'PackageManifest',
	#superclass : 'Object',
	#category : 'Kernel-Manifest',
	#package : 'Kernel',
	#tag : 'Manifest'
}

{ #category : 'accessing - comment' }
PackageManifest class >> classCommentBlank [

	^'No description for this package available. Please add a description for this package here'
]

{ #category : 'meta-data' }
PackageManifest class >> description [
	^ 'No description for this package manifest available. Please add a description for this package here.'
]

{ #category : 'meta-data - dependency analyser' }
PackageManifest class >> ignoredDependencies [
	^ #(#'System-Settings-Core')
]

{ #category : 'deprecation' }
PackageManifest class >> isDeprecated [
	^ false
]

{ #category : 'testing' }
PackageManifest class >> isManifest [

	^ self ~= PackageManifest
]

{ #category : 'System-Support' }
PackageManifest class >> isUsed [
	^ true
]

{ #category : 'meta-data - dependency analyser' }
PackageManifest class >> manuallyResolvedDependencies [
	^ #()
]

{ #category : 'unloading' }
PackageManifest class >> postUnload [
	"Override this method to specify clean-up actions that should be done after package unloading."

	self postUnloadAction ifNotNil: [ :block |
		Warning signal:
			'#postUnloadAction should be replaced by #postUnload doing the actual action instead of returning a block. This branch should be removed in Pharo 13 to keep only the empty post unload method.'.
		block value ]
]

{ #category : 'unloading' }
PackageManifest class >> postUnloadAction [
	"This method should not be overriden and should be removed in Pharo 13"

	^ nil
]

{ #category : 'unloading' }
PackageManifest class >> preUnload [
	"Override this method to specify clean-up actions that should be done before package unloading."

	
]

{ #category : 'code-critics' }
PackageManifest class >> rejectClasses [
	^ #()
]

{ #category : 'code-critics' }
PackageManifest class >> rejectRules [
	^ #()
]
