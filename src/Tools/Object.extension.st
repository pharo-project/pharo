Extension { #name : 'Object' }

{ #category : '*Tools' }
Object >> basicInspect [
	"Create and schedule an Inspector in which the user can examine the receiver's variables.
	Return the receiver. For getting the inspector, use #basicInspector"
	(Smalltalk tools toolNamed: #basicInspector) inspect: self
]

{ #category : '*Tools' }
Object >> basicInspector [
	"Create and schedule a basic Inspector in which the user can examine the receiver's variables.
	Return the inspector. For getting the object itself, use #basicInspect"
	^ (Smalltalk tools toolNamed: #basicInspector) inspect: self
]

{ #category : '*Tools' }
Object >> browse [
	
	^ (Smalltalk tools toolNamed: #browser) openOnClass: self class
]

{ #category : '*Tools' }
Object >> browseHierarchy [

	self systemNavigation browseHierarchy: self class
]

{ #category : '*Tools' }
Object >> inspect [
	"Create and schedule an Inspector in which the user can examine the receiver's variables.
	Return the receiver. For getting the inspector, use #inspector"
	"https://github.com/pharo-project/pharo/issues/11803"

	(Smalltalk tools toolNamed: #inspector) inspect: self
]

{ #category : '*Tools' }
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

{ #category : '*Tools' }
Object >> inspectWithLabel: aLabel [
	"Create and schedule an Inspector in which the user can examine the receiver's variables."

	^ (Smalltalk tools toolNamed: #inspector) inspect: self label: aLabel
]

{ #category : '*Tools' }
Object >> inspector [

	"Create and schedule an Inspector in which the user can examine the receiver's variables.
	Return the inspector. For getting the object itself, use >>inspect"

	"https://github.com/pharo-project/pharo/issues/11803"

	^ (Smalltalk tools toolNamed: #inspector) inspect: self
]

{ #category : '*Tools-FileList' }
Object class >> services [
	"Backstop"
	^#()
]
