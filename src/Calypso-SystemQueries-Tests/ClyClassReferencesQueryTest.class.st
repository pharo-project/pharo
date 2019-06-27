Class {
	#name : #ClyClassReferencesQueryTest,
	#superclass : #ClyMethodQueryTestCase,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #'methods for tests' }
ClyClassReferencesQueryTest class >> classSideReference1OfClass [

	^ClyClassReferencesQueryTest 
]

{ #category : #running }
ClyClassReferencesQueryTest >> createQuery [
	^ ClyClassReferencesQuery
		to: self referencedClass
		from: (ClyClassScope of: Object in: environment)
]

{ #category : #'methods for tests' }
ClyClassReferencesQueryTest >> reference1OfClass [

	^ClyClassReferencesQueryTest 
]

{ #category : #'methods for tests' }
ClyClassReferencesQueryTest >> reference2OfClass [

	^ClyClassReferencesQueryTest 
]

{ #category : #'methods for tests' }
ClyClassReferencesQueryTest >> referenceOfAnotherClass [

	^Point
]

{ #category : #'methods for tests' }
ClyClassReferencesQueryTest >> referenceOfNumberClassByNameSymbol [

	^#Number
]

{ #category : #'methods for tests' }
ClyClassReferencesQueryTest >> referenceOfSmalltalk [

	^Smalltalk
]

{ #category : #running }
ClyClassReferencesQueryTest >> referencedClass [
	^self class
]

{ #category : #running }
ClyClassReferencesQueryTest >> setUp [
	super setUp.
	
	environment system: ClySystemEnvironment currentImage.
	environment detachFromSystem.
]

{ #category : #tests }
ClyClassReferencesQueryTest >> testCheckIfEmpty [
	query scope: (ClyMethodScope
		of: (self class >> #reference1OfClass) in: environment).
	self deny: query checkEmptyResult.
	
	query scope: (ClyMethodScope
		of: (self class >> #referenceOfAnotherClass) in: environment).
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyClassReferencesQueryTest >> testComparison [

	self 
		assert: (ClyClassReferencesQuery to: Point) 
		equals: (ClyClassReferencesQuery to: Point).
	
	self 
		assert: (ClyClassReferencesQuery to: Point) hash 
		equals: (ClyClassReferencesQuery to: Point) hash.
		
	self deny: (ClyClassReferencesQuery to: Point)
					 = (ClyClassReferencesQuery to: Rectangle)
]

{ #category : #tests }
ClyClassReferencesQueryTest >> testDescriptionWhenSimpleClassBindingIsUsed [

	query := ClyClassReferencesQuery to: Object binding.
	
	self assert: query description equals: 'references to Object'
]

{ #category : #tests }
ClyClassReferencesQueryTest >> testForGlobalVariable [

	query variableQuery: (ClyConstantQuery returning: {self class environment bindingOf: #Smalltalk}).
	self queryFromScope: ClyMethodScope of: self class >> #referenceOfSmalltalk.
	
	self assert: resultItems size equals: 1.
	self assert: resultItems first == (self class >> #referenceOfSmalltalk)
]

{ #category : #tests }
ClyClassReferencesQueryTest >> testForMetaclass [

	query := ClyClassReferencesQuery to: self referencedClass classSide.
	
	self queryFromScope: ClyMethodScope of: self class >> #reference1OfClass.
	
	self assert: resultItems size equals: 1.
	self assert: resultItems first == (self class >> #reference1OfClass)
]

{ #category : #tests }
ClyClassReferencesQueryTest >> testFromClassScope [

	self queryFromScope: ClyBothMetaLevelClassScope of: self class.
	
	self assert: resultItems size equals: 3.	
	self assert: foundSelectors equals: #(classSideReference1OfClass reference1OfClass reference2OfClass)
]

{ #category : #tests }
ClyClassReferencesQueryTest >> testFromMethodScope [

	self 
		queryFromScope: ClyMethodScope
		ofAll: { self class >> #reference1OfClass. self class >> #referenceOfAnotherClass }.
	
	self assert: resultItems size equals: 1.
	self assert: resultItems first == (self class >> #reference1OfClass)
]

{ #category : #tests }
ClyClassReferencesQueryTest >> testFromPackageScope [

	self queryFromScope: ClyPackageScope of: self class package.
	
	self assert: resultItems size equals: 3.	
	self assert: foundSelectors equals: #(classSideReference1OfClass reference1OfClass reference2OfClass)
]

{ #category : #tests }
ClyClassReferencesQueryTest >> testFromSystemScope [

	self queryFromScope: ClySystemEnvironmentScope of: ClySystemEnvironment currentImage.
	
	self assert: resultItems size equals: 3.	
	self assert: foundSelectors equals: #(classSideReference1OfClass reference1OfClass reference2OfClass)
]

{ #category : #tests }
ClyClassReferencesQueryTest >> testShouldFoundClassNameSymbolReference [

	query variableQuery: (ClyConstantQuery returning: { Number }).
	self 
		queryFromScope: ClyMethodScope
		of: self class >> #referenceOfNumberClassByNameSymbol.
	
	self assert: resultItems size equals: 1.
	self assert: resultItems first == (self class >> #referenceOfNumberClassByNameSymbol)
]
