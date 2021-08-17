"
I contain all basic collections.
"
Class {
	#name : #CollectionsAssembly,
	#superclass : #AssemblyManifest,
	#category : #'Deprecated10-Assemblies'
}

{ #category : #'meta-data' }
CollectionsAssembly class >> description [
	^ 'I contain all basic collections.'
]

{ #category : #'meta-data' }
CollectionsAssembly class >> packageName [
	^ #Collections
]

{ #category : #'meta-data' }
CollectionsAssembly class >> parts [
	^ {#'Collections-Abstract' -> #().
	#'Collections-Arithmetic' -> #().
	#'Collections-Atomic' -> #().
	#'Collections-Native' -> #().
	#'Collections-Sequenceable' -> #().
	#'Collections-Stack' -> #().
	#'Collections-Streams' -> #().
	#'Collections-Strings' -> #().
	#'Collections-Support' -> #().
	#'Collections-Unordered' -> #().
	#'Collections-Weak' -> #()}
]

{ #category : #'meta-data' }
CollectionsAssembly class >> version [
	^ '1.0-dev'
]
