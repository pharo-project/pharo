"
Debugger from the Glamarous Toolkit (GT)
"
Class {
	#name : #ManifestGTDebugger,
	#superclass : #PackageManifest,
	#category : #'GT-Debugger-Manifest'
}

{ #category : #testing }
ManifestGTDebugger class >> isDeprecated [
	^ true
]

{ #category : #unloading }
ManifestGTDebugger class >> preUnload [
  GTGenericStackDebugger closeAllDebuggers
]
