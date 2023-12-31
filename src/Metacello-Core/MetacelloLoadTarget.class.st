Class {
	#name : 'MetacelloLoadTarget',
	#superclass : 'MetacelloTarget',
	#category : 'Metacello-Core-Targets',
	#package : 'Metacello-Core',
	#tag : 'Targets'
}

{ #category : 'accessing' }
MetacelloLoadTarget >> actionLabel [

	^'Loading '
]

{ #category : 'accessing' }
MetacelloLoadTarget >> loader [
	
	^ engine loader
]

{ #category : 'visiting' }
MetacelloLoadTarget >> visitAtomicLoadDirective: aMetacelloAtomicLoadDirective [ 
	
	| atomicLoader |
	atomicLoader := MetacelloAtomicLoad new.
	atomicLoader target: self.
	aMetacelloAtomicLoadDirective loadDirectives do: [ :dir |
		dir acceptVisitor: atomicLoader ].
	atomicLoader finishLoading.
]

{ #category : 'visiting' }
MetacelloLoadTarget >> visitLinearLoadDirective: aMetacelloLinearLoadDirective [

	aMetacelloLinearLoadDirective loadDirectives do: [ :dir |
		dir acceptVisitor: self ]
]

{ #category : 'visiting' }
MetacelloLoadTarget >> visitPackageLoadDirective: aMetacelloPackageLoadDirective [

	engine loader loadPackageDirective: aMetacelloPackageLoadDirective
]

{ #category : 'visiting' }
MetacelloLoadTarget >> visitPostLoadDirective: aMetacelloPostLoadDirective [
	
	aMetacelloPostLoadDirective spec postLoadDoItBlock ifNotNil: [ :block |
		block valueWithPossibleArgs: {
				self.
				aMetacelloPostLoadDirective spec } ]
]

{ #category : 'visiting' }
MetacelloLoadTarget >> visitPreLoadDirective: aMetacelloPreLoadDirective [
	
	aMetacelloPreLoadDirective spec preLoadDoItBlock ifNotNil: [ :block |
		block valueWithPossibleArgs: {
				self.
				aMetacelloPreLoadDirective spec } ]
]
