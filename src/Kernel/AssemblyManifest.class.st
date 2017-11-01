"
I store meta-data for a package assembly. I'm the common superclass of all package assemblies.
"
Class {
	#name : #AssemblyManifest,
	#superclass : #Object,
	#category : 'Kernel'
}

{ #category : #accessing }
AssemblyManifest class >> description [
	^ self name , ' description'
]

{ #category : #'compatibility - package manifest' }
AssemblyManifest class >> ignoredDependencies [
	^ #()
]

{ #category : #default }
AssemblyManifest class >> manifestClassComment [
	^ 'I store metadata for this package assembly.'
]

{ #category : #'instance creation' }
AssemblyManifest class >> newNamed: aString category: aCategoryName [
	| assemblyClass |
	assemblyClass := self subclass: aString asSymbol
			instanceVariableNames: ''
			classVariableNames: ''
			poolDictionaries: ''
			category: aCategoryName.
	assemblyClass comment: self manifestClassComment.
	^ assemblyClass
]

{ #category : #accessing }
AssemblyManifest class >> packageName [
	self shouldBeImplemented
]

{ #category : #accessing }
AssemblyManifest class >> parts [
	^ #()
]

{ #category : #accessing }
AssemblyManifest class >> version [
	^ '0.1-dev'
]
