Class {
	#name : #ManifestMonticello,
	#superclass : #PackageManifest,
	#category : #Monticello
}

{ #category : #'meta-data - dependency analyser' }
ManifestMonticello class >> ignoredDependencies [
	^ #(#'Graphics-Primitives' #'FFI-Kernel' #'System-Settings-Core')
]

{ #category : #'meta-data - dependency analyser' }
ManifestMonticello class >> manuallyResolvedDependencies [
	^ #(#'Graphics-Primitives' #'OpalCompiler-Core' #'System-Localization' #Jobs #'Random-Core' #'FFI-Kernel' #'Ring-Deprecated-Monticello' #'System-Settings-Core' #'FileSystem-Disk')
]
