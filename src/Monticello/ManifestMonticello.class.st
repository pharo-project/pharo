"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
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
	^ #(#'Graphics-Primitives' #'OpalCompiler-Core' #'System-Localization' #Jobs #'Random-Core' #'FFI-Kernel' #'System-Settings' #'FileSystem-Disk')
]
