"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestGTDebugger,
	#superclass : #PackageManifest,
	#category : #'GT-Debugger'
}

{ #category : #unloading }
ManifestGTDebugger class >> preUnload [
  GTGenericStackDebugger closeAllDebuggers.
  GTGenericStackDebugger setGTDebuggerEnabledStatus: false.
]
