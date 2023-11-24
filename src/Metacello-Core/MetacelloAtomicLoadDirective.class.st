Class {
	#name : 'MetacelloAtomicLoadDirective',
	#superclass : 'MetacelloVersionLoadDirective',
	#instVars : [
		'packageloads',
		'preloads',
		'postloads'
	],
	#category : 'Metacello-Core-Directives',
	#package : 'Metacello-Core',
	#tag : 'Directives'
}

{ #category : 'instance creation' }
MetacelloAtomicLoadDirective >> newLoadingStrategyFrom: oldLoadingStrategy [

	^ oldLoadingStrategy changeToAtomic
]

{ #category : 'actions' }
MetacelloAtomicLoadDirective >> newWithEngine: aLoadPolicy [

	| gofer |
	gofer := Gofer new.
	gofer disablePackageCache.
	gofer repository: aLoadPolicy cacheRepository.
	^ MetacelloAtomicLoadingStrategy new
		gofer: gofer;
		yourself
]

{ #category : 'accessing' }
MetacelloAtomicLoadDirective >> title [

	^'atomic load'
]
