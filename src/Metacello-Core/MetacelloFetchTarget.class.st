"
I am the fetch target: I visit the load directives the record target built and fetch, through the engine's loader, what each package directive needs, without loading anything.
"
Class {
	#name : 'MetacelloFetchTarget',
	#superclass : 'MetacelloTarget',
	#category : 'Metacello-Core-Targets',
	#package : 'Metacello-Core',
	#tag : 'Targets'
}

{ #category : 'visiting' }
MetacelloFetchTarget >> visitAtomicLoadDirective: aMetacelloAtomicLoadDirective [

	aMetacelloAtomicLoadDirective loadDirectives do: [ :dir | dir acceptVisitor: self ]
]

{ #category : 'visiting' }
MetacelloFetchTarget >> visitLinearLoadDirective: aMetacelloLinearLoadDirective [

	aMetacelloLinearLoadDirective loadDirectives do: [ :dir | dir acceptVisitor: self ]
]

{ #category : 'visiting' }
MetacelloFetchTarget >> visitPackageLoadDirective: aMetacelloPackageLoadDirective [

	engine loader fetchPackageDirective: aMetacelloPackageLoadDirective
]

{ #category : 'visiting' }
MetacelloFetchTarget >> visitPostLoadDirective: aMetacelloPostLoadDirective [ 
	
	"Nothing"
]

{ #category : 'visiting' }
MetacelloFetchTarget >> visitPreLoadDirective: aMetacelloPostLoadDirective [ 
	
	"Nothing"
]
