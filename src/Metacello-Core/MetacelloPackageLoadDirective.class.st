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

{ #category : 'comparing' }
MetacelloPackageLoadDirective >> hash [

	^ self packageName hash
]

{ #category : 'printing' }
MetacelloPackageLoadDirective >> label [

	^ spec name , ' -- ' , self repositorySpecs first fullDescription
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
MetacelloPackageLoadDirective >> repository [

	^self repositorySpecs repository
]

{ #category : 'accessing' }
MetacelloPackageLoadDirective >> repositorySpecs [

	^ repositorySpecs ifNil: [ repositorySpecs := self spec ifNotNil: [ self spec repositorySpecs ] ]
]

{ #category : 'accessing' }
MetacelloPackageLoadDirective >> repositorySpecs: aSpec [

	repositorySpecs := aSpec
]

{ #category : 'asserting' }
MetacelloPackageLoadDirective >> shouldLoadStrict [
	
	^ spec shouldLoadStrict
]

{ #category : 'accessing' }
MetacelloPackageLoadDirective >> title [

	^ 'load'
]
