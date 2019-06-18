"
My subclasses represent the scope of concrete typed objects.
I do not require them to implement any specific methods. They just define logically what objects they represent. And they should provide methods to assess information from them.

For example there is ClyClassScope which is based on set of classes. And there is ClyPackageScope which is based on set of packages.
And they provide methods which queries are using to retrieve required objects: 
	aClassScope classesDo: aBlock
	aClassScope methodsDo: aBlock 
	aPackageScope packagesDo: aBlock 
	aPackageScope classesDo: aBlock 
	aPackageScope methodsDo: aBlock

I provide several methods to instantiate typed scopes:

	ClyClassScope of: String.
	ClyClassScope of: String in: ClyNavigationEnvironment currentImage.
	ClyClassScope of: String in: ClyNavigationEnvironment currentImage named: 'String scope'.

They create class scopes based on single basis object String.

	ClyClassScope ofAll: {String. Point}.
	ClyClassScope of: {String. Point} in: ClyNavigationEnvironment currentImage.
	ClyClassScope of: {String. Point} in: ClyNavigationEnvironment currentImage named: 'String and Point'.

They create scopes of two classes String and Point.

Also users can ask for empty scope: 

	ClyClassScope empty.
	ClyClassScope emptyIn: ClyNavigationEnvironment.
	
I implement several methods convert existing scopes to new one with modified basis:

- withNewBasisObjects: newBasisObjects
It returns new scope similar to receiver but with basis.

- withExtraBasisObject: extraBasisObject 
It returnes new scope similar to receiver but with basis extended by given extraBasisObject.

- withExtraBasisObjects: extraBasisObjects 
It returnes new scope similar to receiver but with basis extended by all extraBasisObjects.

- withoutBasisObject: existingBasisObject 
It returnes new scope similar to receiver but with basis which excludes existingBasisObject. It ignores the case when given object is not in the basis of receiver scope.

- withoutBasisObjects: existingBasisObjects 
It returnes new scope similar to receiver but with basis which excludes all existingBasisObjects. It ignores the case when some of given objects are not in the basis of receiver scope.

- restrictedBy: anotherScope
It returnes new scope similar to receiver but with basis of given anotherScope

Also I provide scope composition method which merges two scopes: 
	
	(ClyClassScope of: String) , (ClyClassScope of: Array)
	
It returns ClyCompositeScope instance. 

To support composite scope and query I introduce method #asUnifiedInstance which supposed to return similar scope with same kind of scope class and internal parameters but with empty basis.
So any possible instance of my subclass should produce equal unified instance with this method. It is used to merge subqueries and their scopes when ClyUnionQuery is built. 
	
Also I implement my superclass abstract methods like: 
- supportsQuery: aQuery. It returns true for any kind of ClyTypedQuery.
- representsScope: aScopeClass. It just checks if receiver is kind of given scope class.
- adoptQuery: aQuery. It just assigns receiver to the aQuery to be it scope.

"
Class {
	#name : #ClyTypedScope,
	#superclass : #ClyScope,
	#category : #'Calypso-NavigationModel-Model'
}

{ #category : #'instance creation' }
ClyTypedScope class >> empty [
	^ self ofAll: #()
]

{ #category : #'instance creation' }
ClyTypedScope class >> emptyIn: aNavigationEnvironment [
	^ self ofAll: #() in: aNavigationEnvironment
]

{ #category : #'instance creation' }
ClyTypedScope class >> of: aBasisObject [
		
	^self ofAll: {aBasisObject}
]

{ #category : #'instance creation' }
ClyTypedScope class >> of: aBasisObject in: aNavigationEnvironment [

	^self ofAll: {aBasisObject} in: aNavigationEnvironment
]

{ #category : #'instance creation' }
ClyTypedScope class >> of: aBasisObject in: aNavigationEnvironment named: aString [

	^(self of: aBasisObject in: aNavigationEnvironment)
		name: aString
]

{ #category : #'instance creation' }
ClyTypedScope class >> ofAll: basisObjects [

	^self new
		basisObjects: basisObjects
]

{ #category : #'instance creation' }
ClyTypedScope class >> ofAll: basisObjects in: aNavigationEnvironment [

	^(self ofAll: basisObjects)
		bindTo: aNavigationEnvironment
]

{ #category : #'instance creation' }
ClyTypedScope class >> ofAll: aBasisObject in: aNavigationEnvironment named: aString [

	^(self ofAll: aBasisObject in: aNavigationEnvironment)
		name: aString
]

{ #category : #composition }
ClyTypedScope >> , anotherTypedScope [
	^ClyCompositeScope on: { self. anotherTypedScope }
]

{ #category : #queries }
ClyTypedScope >> adoptQuery: aQuery [
	aQuery scope: self.
	^aQuery
]

{ #category : #private }
ClyTypedScope >> asUnifiedInstance [
	^self withNewBasisObjects: #()
]

{ #category : #accessing }
ClyTypedScope >> basisObjects: aCollection [
	super basisObjects: (self prepareBasisFrom: aCollection)
]

{ #category : #initialization }
ClyTypedScope >> prepareBasisFrom: aCollection [
	^aCollection
]

{ #category : #testing }
ClyTypedScope >> representsScope: aScopeClass [
	^self isKindOf: aScopeClass
]

{ #category : #converting }
ClyTypedScope >> restrictedBy: aScope [

	^self withNewBasisObjects: aScope basisObjects
]

{ #category : #testing }
ClyTypedScope >> supportsQuery: aQuery [
	^aQuery isKindOf: ClyTypedQuery 
]

{ #category : #converting }
ClyTypedScope >> withExtraBasisObject: newBasisObject [

	(basisObjects includes: newBasisObject) ifTrue: [ ^self ].
	 
	^self withNewBasisObjects: (basisObjects copyWith: newBasisObject)
]

{ #category : #converting }
ClyTypedScope >> withExtraBasisObjects: extraBasisObjects [

	| newBasis |
	newBasis := OrderedCollection withAll: basisObjects.
	extraBasisObjects do: [ :each | 
		(newBasis identityIncludes: each) ifFalse: [ newBasis add: each ] ].

	^self withNewBasisObjects: newBasis
]

{ #category : #converting }
ClyTypedScope >> withNewBasisObjects: newBasisObjects [

	^self copy 
		basisObjects: newBasisObjects;
		bindTo: environment
]

{ #category : #converting }
ClyTypedScope >> withoutBasisObject: existingBasisObject [

	^self withNewBasisObjects: (basisObjects copyWithout: existingBasisObject)
]

{ #category : #converting }
ClyTypedScope >> withoutBasisObjects: existingBasisObjects [

	^self withNewBasisObjects: (basisObjects copyWithoutAll: existingBasisObjects)
]
