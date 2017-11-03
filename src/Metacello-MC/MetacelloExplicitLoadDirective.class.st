Class {
	#name : #MetacelloExplicitLoadDirective,
	#superclass : #MetacelloVersionLoadDirective,
	#category : #Metacello-MC-Directives
}

{ #category : #actions }
MetacelloExplicitLoadDirective >> explicitLoadUsing: aLoaderDirective gofer: aGofer [

	aLoaderDirective loadLinearLoadDirective: self gofer: aGofer.
]

{ #category : #actions }
MetacelloExplicitLoadDirective >> explicitLoadWithPolicy: aLoadPolicy [

	| gofer |
	gofer := MetacelloGofer new.
	gofer disablePackageCache.
	gofer repository: aLoadPolicy cacheRepository.
	self explicitLoadUsing: self gofer: gofer
]

{ #category : #testing }
MetacelloExplicitLoadDirective >> isExplicit [

	^true
]

{ #category : #actions }
MetacelloExplicitLoadDirective >> loadUsing: aLoaderDirective gofer: aGofer [

	aLoaderDirective loadExplicitLoadDirective: self gofer: aGofer.
]

{ #category : #accessing }
MetacelloExplicitLoadDirective >> title [

	^'explicit load'
]
