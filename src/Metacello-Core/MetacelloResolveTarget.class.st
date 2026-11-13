"
I am the resolve target, the first of the chain: I choose the version of every project a load reaches, with the engine's resolver (MetacelloInteractiveResolver unless the script names another), and answer the MetacelloResolution the other targets follow. I load and register nothing.
- resolve: resolves a load from a project spec (a script's root).
"
Class {
	#name : 'MetacelloResolveTarget',
	#superclass : 'MetacelloTarget',
	#category : 'Metacello-Core-Targets',
	#package : 'Metacello-Core',
	#tag : 'Targets'
}

{ #category : 'resolving' }
MetacelloResolveTarget >> resolve: aProjectSpec [
	"The resolution of loading what aProjectSpec loads (empty for its default groups)"

	^ engine resolver resolve: aProjectSpec engine: engine
]
