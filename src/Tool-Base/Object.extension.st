Extension { #name : #Object }

{ #category : #'*Tool-Base' }
Object >> basicInspect [
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
Object >> explore [
	self
		deprecated: 'Please use #inspect instead'
		transformWith: '`@receiver explore' -> '`@receiver inspect'.
	
	^self inspect
]

{ #category : #'*Tool-Base' }
Object >> inspect [
	"Create and schedule an Inspector in which the user can examine the receiver's variables."
	^ Smalltalk tools inspector inspect: self
]

{ #category : #'*Tool-Base' }
Object >> inspectOnce [
	"Inspect unless we have already done it once."
	
	| node |
	node := thisContext sender sourceNodeExecuted.
	(node hasProperty: #Once) ifTrue: [^self]. 
	node propertyAt: #Once put: true.
	^self inspect
]

{ #category : #'*Tool-Base' }
Object >> inspectWithLabel: aLabel [
	"Create and schedule an Inspector in which the user can examine the receiver's variables."
	^Smalltalk tools inspector inspect: self label: aLabel
]
