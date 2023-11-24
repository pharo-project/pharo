Class {
	#name : 'MetacelloPackageLoadDirective',
	#superclass : 'MetacelloDirective',
	#instVars : [
		'resolvedReference',
		'externalReference'
	],
	#category : 'Metacello-Core-Directives',
	#package : 'Metacello-Core',
	#tag : 'Directives'
}

{ #category : 'actions' }
MetacelloPackageLoadDirective >> executeUsing: aLoader [

	aLoader executePackageDirective: self
]

{ #category : 'accessing' }
MetacelloPackageLoadDirective >> externalReference [
	^ externalReference
]

{ #category : 'accessing' }
MetacelloPackageLoadDirective >> file [

	^self externalReference name
]

{ #category : 'printing' }
MetacelloPackageLoadDirective >> label [

	^self file
]

{ #category : 'enumerating' }
MetacelloPackageLoadDirective >> packageDirectivesDo: aBlock [

	aBlock value: self
]

{ #category : 'accessing' }
MetacelloPackageLoadDirective >> packageName [

	^self externalReference packageName
]

{ #category : 'accessing' }
MetacelloPackageLoadDirective >> repository [

	^self externalReference repository
]

{ #category : 'accessing' }
MetacelloPackageLoadDirective >> resolvedReference [
	^ resolvedReference
]

{ #category : 'accessing' }
MetacelloPackageLoadDirective >> resolvedReference: anObject [
	resolvedReference := anObject
]

{ #category : 'initialization' }
MetacelloPackageLoadDirective >> spec: aPackageSpec externalReference: anExternalReference [

	self spec: aPackageSpec.
	externalReference := anExternalReference
]

{ #category : 'accessing' }
MetacelloPackageLoadDirective >> title [

	^'load'
]
