"
I return all abstract methods inherited by classes in my scope which are still not implemented by them
"
Class {
	#name : #ClyUnimplementedMethods,
	#superclass : #ClyInheritanceAnalysisMethodQuery,
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Queries'
}

{ #category : #accessing }
ClyUnimplementedMethods class >> priority [
	^8.5
]

{ #category : #execution }
ClyUnimplementedMethods >> buildResult: aQueryResult [
	
	| methods |
	methods := OrderedCollection new.
	
	scope classesDo: [ :eachClass | 
		methods addAll: (self methodsToImplementIn: eachClass)].
	
	aQueryResult fillWith: methods
]

{ #category : #execution }
ClyUnimplementedMethods >> checkEmptyResult [

	scope classesDo: [ :eachClass |
		(self isClassHasToBeImplemented: eachClass) ifTrue: [ ^false ]].
	^true
]

{ #category : #printing }
ClyUnimplementedMethods >> description [

	^'unimplemented methods'
]

{ #category : #testing }
ClyUnimplementedMethods >> doesClassDefinedAsAbstract: aClass [
	^(aClass classSide includesLocalSelector: #isAbstract) and: [aClass instanceSide isAbstract]
]

{ #category : #testing }
ClyUnimplementedMethods >> isAbstractMethod: aMethod [
	^aMethod sendsSelector: #subclassResponsibility
]

{ #category : #testing }
ClyUnimplementedMethods >> isAffectedByChangedMethod: aMethod [
	| methodClass selector |
	selector := aMethod selector.
	methodClass := aMethod origin.
	
	(self isFromScopeWithClassKindOf: methodClass) ifFalse: [ ^false ].
	
	^(self isAbstractMethod: aMethod) or: [self isClass: methodClass shouldImplement: selector]
]

{ #category : #testing }
ClyUnimplementedMethods >> isClass: aClass shouldImplement: aSelector [

	| inheritedMethod |
	aClass isRootInEnvironment ifTrue: [ ^false].
	
	inheritedMethod := aClass superclass lookupSelector: aSelector.
	inheritedMethod ifNil: [ ^false ].
	
	^self isAbstractMethod: inheritedMethod
]

{ #category : #testing }
ClyUnimplementedMethods >> isClassHasToBeImplemented: aClass [
	(self doesClassDefinedAsAbstract: aClass) ifTrue: [ ^false].
	
	aClass allSuperclassesDo: [ :superclass | 
		superclass == Object ifTrue: [ ^false ].
		
		self withAbstractMethodsOf: superclass do: [:abstractMethod | | implementor |
			implementor := aClass lookupSelector: abstractMethod selector.
			implementor == abstractMethod ifTrue: [ ^true]]].
	
	^false
]

{ #category : #execution }
ClyUnimplementedMethods >> methodsToImplementIn: aClass [
	| result implementor |
	(self doesClassDefinedAsAbstract: aClass) ifTrue: [ ^#()].
		
	result := OrderedCollection new.
	aClass allSuperclassesDo: [ :superclass | 
		superclass == Object ifTrue: [ ^result ].

		self withAbstractMethodsOf: superclass do: [ :abstractMethod |  
			implementor := aClass lookupSelector: abstractMethod selector.
			implementor == abstractMethod ifTrue: [ result add: implementor ]]			
	].	
	^result
]

{ #category : #testing }
ClyUnimplementedMethods >> selectsMethod: aMethod [

	| selector |
	selector := aMethod selector.
	
	scope classesDo: [:eachClass | 
		((self isClass: eachClass shouldImplement: selector) 
			and: [(eachClass includesSelector: selector) not])
				ifTrue: [ ^true ]].
	^false
]

{ #category : #execution }
ClyUnimplementedMethods >> withAbstractMethodsOf: aClass do: aBlock [

	aClass localMethods 
		select: [ :each | self isAbstractMethod: each ]
		thenDo: aBlock
]
