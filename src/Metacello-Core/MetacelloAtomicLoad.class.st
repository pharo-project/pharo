Class {
	#name : 'MetacelloAtomicLoad',
	#superclass : 'Object',
	#instVars : [
		'target',
		'packageDirectives'
	],
	#category : 'Metacello-Core-Targets',
	#package : 'Metacello-Core',
	#tag : 'Targets'
}

{ #category : 'loading' }
MetacelloAtomicLoad >> finishLoading [

	target loader loadAtomicPackageDirectives: packageDirectives
]

{ #category : 'visiting' }
MetacelloAtomicLoad >> initialize [

	super initialize.
	packageDirectives := OrderedCollection new
]

{ #category : 'accessing' }
MetacelloAtomicLoad >> target: aTarget [

	target := aTarget
]

{ #category : 'visiting' }
MetacelloAtomicLoad >> visitPackageLoadDirective: aMetacelloPackageLoadDirective [ 
	
	packageDirectives add: aMetacelloPackageLoadDirective
]
