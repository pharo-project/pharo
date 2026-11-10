"
I am the resolve target, the first of the chain: I choose the version of every project a load reaches, with the engine's resolver (MetacelloInteractiveResolver unless the script names another), and answer the MetacelloResolution the other targets follow. I load and register nothing.

- resolve:loads: resolves a load from a project spec (a script's root);
- resolveVersion:loads: resolves from a version spec recorded on its own.
"
Class {
	#name : 'MetacelloResolveTarget',
	#superclass : 'MetacelloTarget',
	#category : 'Metacello-Core-Targets',
	#package : 'Metacello-Core',
	#tag : 'Targets'
}

{ #category : 'resolving' }
MetacelloResolveTarget >> resolve: aProjectSpec loads: aCollectionOfNames [
	"The resolution of loading aCollectionOfNames (empty for the default ones) from aProjectSpec"

	^ engine resolver resolve: aProjectSpec loads: aCollectionOfNames engine: engine
]

{ #category : 'resolving' }
MetacelloResolveTarget >> resolveVersion: aVersionSpec loads: aCollectionOfNames [
	"The resolution of the projects loading aCollectionOfNames from aVersionSpec reaches, for a version chosen without the engine"

	^ engine resolver resolveVersion: aVersionSpec loads: aCollectionOfNames engine: engine
]
