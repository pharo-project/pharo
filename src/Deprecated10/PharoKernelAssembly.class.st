"
I store metadata for this package assembly.
"
Class {
	#name : #PharoKernelAssembly,
	#superclass : #AssemblyManifest,
	#category : #'Deprecated10-Assemblies'
}

{ #category : #'meta-data' }
PharoKernelAssembly class >> description [
	^ 'I represent the minimal kernel, i.e. the minimum set of packages needed, to get a usable Pharo image.'
]

{ #category : #'meta-data' }
PharoKernelAssembly class >> packageName [
	^ #'Pharo-Kernel'
]

{ #category : #'meta-data' }
PharoKernelAssembly class >> parts [
	^ {
		(#Kernel -> #()) .
		(#Monticello -> #()) .
		(#'PharoBootstrap-Initialization' -> #()) .
		(#'Transcript-NonInteractive' -> #())
	}
]

{ #category : #'meta-data' }
PharoKernelAssembly class >> version [
	^ '1.0-dev'
]
