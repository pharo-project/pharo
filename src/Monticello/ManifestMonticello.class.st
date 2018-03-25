Class {
	#name : #ManifestMonticello,
	#superclass : #PackageManifest,
	#category : #Monticello
}

{ #category : #'meta-data - dependency analyser' }
ManifestMonticello class >> ignoredDependencies [
	^ #(#'Graphics-Primitives' #'FFI-Kernel' #'System-Settings')
]

{ #category : #'meta-data - dependency analyser' }
ManifestMonticello class >> manuallyResolvedDependencies [
	^ #(#'Graphics-Primitives' #'OpalCompiler-Core' #'System-Localization' #Jobs #'Random-Core' #'FFI-Kernel' #'Ring-Monticello' #'System-Settings' #'FileSystem-Disk')
]
