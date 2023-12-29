Class {
	#name : 'MetacelloSpecBuilder',
	#superclass : 'Object',
	#instVars : [
		'currentLoadDirective',
		'directives',
		'root'
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
	root := currentLoadDirective := MetacelloLinearLoadDirective new
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

	self
		pushLoadDirective: root
		during: aBlock
]

{ #category : 'private' }
MetacelloSpecBuilder >> pushLoadDirective: aLoaderDirective during: aBlock [

	| oldRoot |
	oldRoot := currentLoadDirective.
	currentLoadDirective := aLoaderDirective.
	aBlock ensure: [ currentLoadDirective := oldRoot ]
]

{ #category : 'accessing' }
MetacelloSpecBuilder >> root [
	^ root
]
