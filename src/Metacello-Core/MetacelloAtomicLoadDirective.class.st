Class {
	#name : 'MetacelloAtomicLoadDirective',
	#superclass : 'MetacelloVersionLoadDirective',
	#category : 'Metacello-Core-Directives',
	#package : 'Metacello-Core',
	#tag : 'Directives'
}

{ #category : 'testing' }
MetacelloAtomicLoadDirective >> includes: aMetacelloPackageLoadDirective [ 
	
	^ self loadDirectives includes: aMetacelloPackageLoadDirective
]

{ #category : 'accessing' }
MetacelloAtomicLoadDirective >> title [

	^'atomic load'
]
