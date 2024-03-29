Class {
	#name : 'MetacelloAtomicLoad',
	#superclass : 'Object',
	#instVars : [
		'target',
		'packageDirectives',
		'postLoads',
		'preLoads'
	],
	#category : 'Metacello-Core-Targets',
	#package : 'Metacello-Core',
	#tag : 'Targets'
}

{ #category : 'loading' }
MetacelloAtomicLoad >> finishLoading [

	"Execute all preloads, then atomically all packages, finally all preloads"
	
	preLoads do: [ :e | target visitPreLoadDirective: e ].
	preLoads := OrderedCollection new.

	target loader loadAtomicPackageDirectives: packageDirectives.
	packageDirectives := OrderedCollection new.
	
	postLoads do: [ :e | target visitPostLoadDirective: e ].
	postLoads := OrderedCollection new.
]

{ #category : 'initialization' }
MetacelloAtomicLoad >> initialize [

	super initialize.
	packageDirectives := OrderedCollection new.
	preLoads := OrderedCollection new.
	postLoads := OrderedCollection new.
]

{ #category : 'accessing' }
MetacelloAtomicLoad >> target: aTarget [

	target := aTarget
]

{ #category : 'visiting' }
MetacelloAtomicLoad >> visitAtomicLoadDirective: aMetagelloAtomicLoadDirective [

	"An atomic inside an atomic just merges the two atomic into one"
	aMetagelloAtomicLoadDirective loadDirectives do: [ :dir |
		dir acceptVisitor: self ].
]

{ #category : 'visiting' }
MetacelloAtomicLoad >> visitLinearLoadDirective: aMetacelloLinearLoadDirective [

	"Finish current atomic loading and redispatch the linear loading"
	self finishLoading.
	target visitLinearLoadDirective: aMetacelloLinearLoadDirective
]

{ #category : 'visiting' }
MetacelloAtomicLoad >> visitPackageLoadDirective: aMetacelloPackageLoadDirective [ 
	
	packageDirectives add: aMetacelloPackageLoadDirective
]

{ #category : 'visiting' }
MetacelloAtomicLoad >> visitPostLoadDirective: aMetacelloPostLoadDirective [
	
	postLoads add: aMetacelloPostLoadDirective
]

{ #category : 'visiting' }
MetacelloAtomicLoad >> visitPreLoadDirective: aMetacelloPreLoadDirective [
	
	preLoads add: aMetacelloPreLoadDirective
]
