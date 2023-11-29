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
	preLoads := #().

	target loader loadAtomicPackageDirectives: packageDirectives.
	packageDirectives := #().
	
	postLoads do: [ :e | target visitPostLoadDirective: e ].
	postLoads := #().
]

{ #category : 'visiting' }
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
