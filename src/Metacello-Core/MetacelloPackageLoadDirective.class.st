Class {
	#name : 'MetacelloPackageLoadDirective',
	#superclass : 'MetacelloDirective',
	#instVars : [
		'repositorySpecs'
	],
	#category : 'Metacello-Core-Directives',
	#package : 'Metacello-Core',
	#tag : 'Directives'
}

{ #category : 'comparing' }
MetacelloPackageLoadDirective >> = anotherPackageDirective [

	anotherPackageDirective species = self species ifFalse: [ ^ false ].
	^ anotherPackageDirective packageName = self packageName
]

{ #category : 'visiting' }
MetacelloPackageLoadDirective >> acceptVisitor: aVisitor [ 
	
	^ aVisitor visitPackageLoadDirective: self
]

{ #category : 'accessing' }
MetacelloPackageLoadDirective >> file [

	^ self repositorySpecs asString
]

{ #category : 'comparing' }
MetacelloPackageLoadDirective >> hash [

	^ self packageName hash
]

{ #category : 'printing' }
MetacelloPackageLoadDirective >> label [

	^ spec name , ' -- ', repositorySpecs first description
]

{ #category : 'enumerating' }
MetacelloPackageLoadDirective >> packageDirectivesDo: aBlock [

	aBlock value: self
]

{ #category : 'accessing' }
MetacelloPackageLoadDirective >> packageName [

	^ self spec name
]

{ #category : 'accessing' }
MetacelloPackageLoadDirective >> packages [
	
	^ { spec }
]

{ #category : 'accessing' }
MetacelloPackageLoadDirective >> repository [

	^self repositorySpecs repository
]

{ #category : 'accessing' }
MetacelloPackageLoadDirective >> repositorySpecs [
	^ repositorySpecs
]

{ #category : 'initialization' }
MetacelloPackageLoadDirective >> spec: aPackageSpec repositorySpecs: anExternalReference [

	self spec: aPackageSpec.
	repositorySpecs := anExternalReference
]

{ #category : 'accessing' }
MetacelloPackageLoadDirective >> title [

	^'load'
]
