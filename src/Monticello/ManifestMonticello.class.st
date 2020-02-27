"
Includes the Monticello source code control system.
"
Class {
	#name : #ManifestMonticello,
	#superclass : #PackageManifest,
	#category : #'Monticello-Manifest'
}

{ #category : #'meta-data - dependency analyser' }
ManifestMonticello class >> ignoredDependencies [
	^ #(#'Graphics-Primitives' #'FFI-Kernel' #'System-Settings-Core')
]

{ #category : #'meta-data - dependency analyser' }
ManifestMonticello class >> manuallyResolvedDependencies [
	^ #(#'Graphics-Primitives' #'OpalCompiler-Core' #Jobs #'Random-Core' #'FFI-Kernel' #'Ring-Definitions-Monticello' #'System-Settings-Core' #'FileSystem-Disk')
]
