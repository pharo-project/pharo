"
I am a Metacello loader for tests: I neither fetch repositories nor load code, I only remember which packages were asked to load and from where.
"
Class {
	#name : 'MetacelloTestLoader',
	#superclass : 'MetacelloMonticelloLoader',
	#instVars : [
		'loaded'
	],
	#category : 'Metacello-Tests-Resolution',
	#package : 'Metacello-Tests',
	#tag : 'Resolution'
}

{ #category : 'loading' }
MetacelloTestLoader >> fetchPackageDirective: aPackageLoadDirective [
	"Nothing to fetch"

	
]

{ #category : 'initialization' }
MetacelloTestLoader >> initialize [

	super initialize.
	loaded := OrderedCollection new
]

{ #category : 'loading' }
MetacelloTestLoader >> loadAtomicPackageDirectives: packageDirectives engine: anEngine [

	packageDirectives do: [ :each |
		loaded add: each spec name -> (each repositorySpecs collect: [ :spec | spec description ] as: Array) ]
]

{ #category : 'loading' }
MetacelloTestLoader >> loaded [
	"Package name -> repository descriptions, in the order they were asked to load"

	^ loaded
]

{ #category : 'loading' }
MetacelloTestLoader >> resolveRepositorySpecs: aCollection [

	^ aCollection
]
