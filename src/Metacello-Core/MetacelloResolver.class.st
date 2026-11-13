"
I decide which version of each project a load uses, before anything is recorded or loaded, and answer a MetacelloResolution. Recording then follows the resolution and decides nothing.

The engine asks the resolver in its options (#resolver, set with Metacello>>#resolver:), MetacelloInteractiveResolver by default. A resolver answers its problems as findings of the resolution; the load goes ahead only when there are no errors.
"
Class {
	#name : 'MetacelloResolver',
	#superclass : 'Object',
	#category : 'Metacello-Core-Resolution',
	#package : 'Metacello-Core',
	#tag : 'Resolution'
}

{ #category : 'resolving' }
MetacelloResolver >> resolve: aProjectSpec engine: anEngine [
	"Answer a MetacelloResolution whose root is the project aProjectSpec names, loading what it loads (empty for its default groups), with every project that load reaches"

	^ self subclassResponsibility
]
