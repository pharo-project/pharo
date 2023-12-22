Class {
	#name : 'MetacelloSpecBuilder',
	#superclass : 'Object',
	#instVars : [
		'currentLoadDirective'
	],
	#category : 'Metacello-Core-Scripts',
	#package : 'Metacello-Core',
	#tag : 'Scripts'
}

{ #category : 'accessing' }
MetacelloSpecBuilder >> currentLoadDirective [
	^ currentLoadDirective
]

{ #category : 'initialization' }
MetacelloSpecBuilder >> initialize [

	super initialize.
	currentLoadDirective := MetacelloLinearLoadDirective new
]

{ #category : 'accessing' }
MetacelloSpecBuilder >> loadDirective [

	^ currentLoadDirective
]

{ #category : 'actions' }
MetacelloSpecBuilder >> pushAtomicLoadDirectivesDuring: aBlock for: aTarget [

	self
		pushLoadDirective: MetacelloAtomicLoadDirective new
		during: aBlock
]

{ #category : 'actions' }
MetacelloSpecBuilder >> pushLinearLoadDirectivesDuring: aBlock for: aTarget [

	| directive |
	directive := MetacelloLinearLoadDirective new.
	self pushLoadDirective: directive during: aBlock.
	^ directive
]

{ #category : 'private' }
MetacelloSpecBuilder >> pushLoadDirective: aLoaderDirective during: aBlock [

	|  oldRoot |
	self loadDirective add: aLoaderDirective.
	oldRoot := currentLoadDirective.
	currentLoadDirective := aLoaderDirective.
	aBlock ensure: [ currentLoadDirective := oldRoot ].
]
