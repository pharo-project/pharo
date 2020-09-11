Extension { #name : #RGBehavior }

{ #category : #'*Ring-RuntimeSupport' }
RGBehavior >> >> aSymbol [

	^ self methodNamed: aSymbol
]

{ #category : #'*Ring-RuntimeSupport' }
RGBehavior >> allSelectors [
	
	^ self allSelectorsWithout: {  }
]

{ #category : #'*Ring-RuntimeSupport' }
RGBehavior >> allSelectorsWithout: behaviors [
	"Returns all the selectors of the receiver and its superclasses, except the ones define in behaviors"

	| selectors |
	selectors := IdentitySet new.
	self withAllSuperclassesDo: [ :class | 
			(behaviors includes: class)
				ifFalse: [ selectors addAll: class selectors ] ].
	^ selectors
]

{ #category : #'*Ring-RuntimeSupport' }
RGBehavior >> allSuperclasses [

	| temp |
	^ ((self superclass == nil) or: [ self superclass == self ])
		ifTrue: [ OrderedCollection new]
		ifFalse: [temp := self superclass allSuperclasses.
			temp addFirst: self superclass.
			temp] 
]

{ #category : #'*Ring-RuntimeSupport' }
RGBehavior >> allSuperclassesDo: aBlock [

	^ self allSuperclasses do: aBlock
]

{ #category : #'*Ring-RuntimeSupport' }
RGBehavior >> binding [

	^ self environment bindingOf: self name
]

{ #category : #'*Ring-RuntimeSupport' }
RGBehavior >> classPool [

	^ Dictionary new.
]

{ #category : #'*Ring-RuntimeSupport' }
RGBehavior >> compile: aString classified: aCategory [ 

	^ self compile: aString classified: aCategory notifying: nil
]

{ #category : #'*Ring-RuntimeSupport' }
RGBehavior >> compile: aString classified: aCategory notifying: aController [

	| parseTree ast selector newMethod | 
	
	parseTree := (RBParser 
		parseMethod: aString
		onError: [ :msg :pos | ^ nil ]) methodClass: self.
		
	ast :=  parseTree doSemanticAnalysisIn: self.  
	
	selector := ast selector.
	newMethod := self ensureLocalMethodNamed: selector.
	newMethod sourceCode: aString.
	aCategory ifNotNil: [ 
		newMethod tagWith: aCategory ].
	
	^ selector.

]

{ #category : #'*Ring-RuntimeSupport' }
RGBehavior >> compile: code notifying: requestor [ 

	| parseTree ast aMethod |
	
	parseTree := (RBParser 
		parseMethod: code 
		onError: [ :msg :pos | "TODO:" ^ nil ]) methodClass: self.
	
	ast := parseTree doSemanticAnalysisIn: self.

	aMethod := self ensureLocalMethodNamed: ast selector.
	aMethod sourceCode: code.
	
	^ aMethod selector
	
]

{ #category : #'*Ring-RuntimeSupport' }
RGBehavior >> isClassSide [
	^self == self classSide
]

{ #category : #'*Ring-RuntimeSupport' }
RGBehavior >> isDefinedInPackage: aPackage [

	^ self package = aPackage
]

{ #category : #'*Ring-RuntimeSupport' }
RGBehavior >> isInstanceSide [
	^self isClassSide not
]

{ #category : #'*Ring-RuntimeSupport' }
RGBehavior >> subclassesDo: aBlock [ 
	"Evaluate the argument, aBlock, for each of the receiver's immediate subclasses."
	
	self subclasses do: aBlock
]

{ #category : #'*Ring-RuntimeSupport' }
RGBehavior >> traits [

	^ self traitComposition traits
]

{ #category : #'*Ring-RuntimeSupport' }
RGBehavior >> withAllSuperclasses [
	"Answer an OrderedCollection of the receiver and the receiver's 
	superclasses. The first element is the receiver, 
	followed by its superclass; the last element is Object."

	| temp |
	temp := self allSuperclasses.
	temp addFirst: self.
	^ temp
]

{ #category : #'*Ring-RuntimeSupport' }
RGBehavior >> withAllSuperclassesDo: aBlock [ 
	"Evaluate the argument, aBlock, for each of the receiver's superclasses."
	aBlock value: self.
	((self superclass == nil) or: [ self superclass == self ])
		ifFalse: [self superclass withAllSuperclassesDo: aBlock]
]
