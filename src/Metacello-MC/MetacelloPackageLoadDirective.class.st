Class {
	#name : #MetacelloPackageLoadDirective,
	#superclass : #MetacelloDirective,
	#instVars : [
		'resolvedReference',
		'externalReference'
	],
	#category : #'Metacello-MC-Directives'
}

{ #category : #accessing }
MetacelloPackageLoadDirective >> externalReference [
	^ externalReference
]

{ #category : #accessing }
MetacelloPackageLoadDirective >> file [

	^self externalReference name
]

{ #category : #printing }
MetacelloPackageLoadDirective >> label [

	^self file
]

{ #category : #actions }
MetacelloPackageLoadDirective >> loadUsing: aLoaderDirective gofer: aGofer [

	aLoaderDirective loadPackageDirective: self gofer: aGofer
]

{ #category : #enumerating }
MetacelloPackageLoadDirective >> packageDirectivesDo: aBlock [

	aBlock value: self
]

{ #category : #enumerating }
MetacelloPackageLoadDirective >> packageDo: aBlock [

	aBlock value: self
]

{ #category : #accessing }
MetacelloPackageLoadDirective >> packageName [

	^self externalReference packageName
]

{ #category : #accessing }
MetacelloPackageLoadDirective >> repository [

	^self externalReference repository
]

{ #category : #accessing }
MetacelloPackageLoadDirective >> resolvedReference [
	^ resolvedReference
]

{ #category : #accessing }
MetacelloPackageLoadDirective >> resolvedReference: anObject [
	resolvedReference := anObject
]

{ #category : #initialization }
MetacelloPackageLoadDirective >> spec: aPackageSpec externalReference: anExternalReference loader: aLoader [

	super spec: aPackageSpec loader: aLoader.
	externalReference := anExternalReference
]

{ #category : #accessing }
MetacelloPackageLoadDirective >> title [

	^'load'
]
