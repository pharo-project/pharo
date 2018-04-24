Class {
	#name : #ManifestGTDebugger,
	#superclass : #PackageManifest,
	#category : #'GT-Debugger'
}

{ #category : #unloading }
ManifestGTDebugger class >> preUnload [
  GTGenericStackDebugger closeAllDebuggers
]
