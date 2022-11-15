Extension { #name : #Object }

{ #category : #'*Tool-Base' }
Object >> basicInspect [
	"Create and schedule an Inspector in which the user can examine the receiver's variables.
	Return the receiver. For getting the inspector, use #basicInspector"
	Smalltalk tools basicInspector inspect: self
]

{ #category : #'*Tool-Base' }
Object >> basicInspector [
	"Create and schedule a basic Inspector in which the user can examine the receiver's variables.
	Return the inspector. For getting the object itself, use #basicInspect"
	^ Smalltalk tools basicInspector inspect: self
]

{ #category : #'*Tool-Base' }
Object >> browse [
	^ Smalltalk tools browser openOnClass: self class
]

{ #category : #'*Tool-Base' }
Object >> browseHierarchy [

	self systemNavigation browseHierarchy: self class
]

{ #category : #'*Tool-Base' }
Object >> inspect [
	"Create and schedule an Inspector in which the user can examine the receiver's variables.
	Return the receiver. For getting the inspector, use #inspector"
	"https://github.com/pharo-project/pharo/issues/11803"

	Smalltalk tools inspector inspect: self
]

{ #category : #'*Tool-Base' }
Object >> inspectOnce [
	"Inspect unless we have already done it once.
	Return the object which was inspected."
	"https://github.com/pharo-project/pharo/issues/11803"

	| node |
	node := thisContext sender sourceNodeExecuted.
	(node hasProperty: #Once) ifTrue: [ ^ self ].
	node propertyAt: #Once put: true.
	self inspect.
	^ self
]

{ #category : #'*Tool-Base' }
Object >> inspectWithLabel: aLabel [
	"Create and schedule an Inspector in which the user can examine the receiver's variables."

	^ Smalltalk tools inspector inspect: self label: aLabel
]
