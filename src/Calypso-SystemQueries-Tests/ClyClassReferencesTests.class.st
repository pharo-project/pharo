Class {
	#name : #ClyClassReferencesTests,
	#superclass : #ClyMethodQueryTestCase,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #'methods for tests' }
ClyClassReferencesTests class >> classSideReference1OfClass [

	^ClyClassReferencesTests 
]

{ #category : #running }
ClyClassReferencesTests >> createQuery [
	^ ClyClassReferences
		to: self referencedClass
		from: (ClyClassScope of: Object in: environment)
]

{ #category : #'methods for tests' }
ClyClassReferencesTests >> reference1OfClass [

	^ClyClassReferencesTests 
]

{ #category : #'methods for tests' }
ClyClassReferencesTests >> reference2OfClass [

	^ClyClassReferencesTests 
]

{ #category : #'methods for tests' }
ClyClassReferencesTests >> referenceOfAnotherClass [

	^Point
]

{ #category : #'methods for tests' }
ClyClassReferencesTests >> referenceOfNumberClassByNameSymbol [

	^#Number
]

{ #category : #'methods for tests' }
ClyClassReferencesTests >> referenceOfSmalltalk [

	^Smalltalk
]

{ #category : #running }
ClyClassReferencesTests >> referencedClass [
	^self class
]

{ #category : #running }
ClyClassReferencesTests >> setUp [
	super setUp.
	
	environment system: ClySystemEnvironment currentImage.
	environment detachFromSystem.
]

{ #category : #tests }
ClyClassReferencesTests >> testCheckIfEmpty [
	query scope: (ClyMethodScope
		of: (self class >> #reference1OfClass) in: environment).
	self deny: query checkEmptyResult.
	
	query scope: (ClyMethodScope
		of: (self class >> #referenceOfAnotherClass) in: environment).
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyClassReferencesTests >> testComparison [

	self 
		assert: (ClyClassReferences to: Point) 
		equals: (ClyClassReferences to: Point).
	
	self 
		assert: (ClyClassReferences to: Point) hash 
		equals: (ClyClassReferences to: Point) hash.
		
	self deny: (ClyClassReferences to: Point)
					 = (ClyClassReferences to: Rectangle)
]

{ #category : #tests }
ClyClassReferencesTests >> testDescriptionWhenSimpleClassBindingIsUsed [

	query := ClyClassReferences to: Object binding.
	
	self assert: query description equals: 'references to Object'
]

{ #category : #tests }
ClyClassReferencesTests >> testForGlobalVariable [

	query variableQuery: (ClyConstantQuery returning: {self class environment bindingOf: #Smalltalk}).
	self queryFromScope: ClyMethodScope of: self class >> #referenceOfSmalltalk.
	
	self assert: resultItems size equals: 1.
	self assert: resultItems first == (self class >> #referenceOfSmalltalk)
]

{ #category : #tests }
ClyClassReferencesTests >> testForMetaclass [

	query := ClyClassReferences to: self referencedClass classSide.
	
	self queryFromScope: ClyMethodScope of: self class >> #reference1OfClass.
	
	self assert: resultItems size equals: 1.
	self assert: resultItems first == (self class >> #reference1OfClass)
]

{ #category : #tests }
ClyClassReferencesTests >> testFromClassScope [

	self queryFromScope: ClyBothMetaLevelClassScope of: self class.
	
	self assert: resultItems size equals: 3.	
	self assert: foundSelectors equals: #(classSideReference1OfClass reference1OfClass reference2OfClass)
]

{ #category : #tests }
ClyClassReferencesTests >> testFromMethodScope [

	self 
		queryFromScope: ClyMethodScope
		ofAll: { self class >> #reference1OfClass. self class >> #referenceOfAnotherClass }.
	
	self assert: resultItems size equals: 1.
	self assert: resultItems first == (self class >> #reference1OfClass)
]

{ #category : #tests }
ClyClassReferencesTests >> testFromPackageScope [

	self queryFromScope: ClyPackageScope of: self class package.
	
	self assert: resultItems size equals: 3.	
	self assert: foundSelectors equals: #(classSideReference1OfClass reference1OfClass reference2OfClass)
]

{ #category : #tests }
ClyClassReferencesTests >> testFromSystemScope [

	self queryFromScope: ClySystemEnvironmentScope of: ClySystemEnvironment currentImage.
	
	self assert: resultItems size equals: 3.	
	self assert: foundSelectors equals: #(classSideReference1OfClass reference1OfClass reference2OfClass)
]

{ #category : #tests }
ClyClassReferencesTests >> testShouldFoundClassNameSymbolReference [

	query variableQuery: (ClyConstantQuery returning: { Number }).
	self 
		queryFromScope: ClyMethodScope
		of: self class >> #referenceOfNumberClassByNameSymbol.
	
	self assert: resultItems size equals: 1.
	self assert: resultItems first == (self class >> #referenceOfNumberClassByNameSymbol)
]
