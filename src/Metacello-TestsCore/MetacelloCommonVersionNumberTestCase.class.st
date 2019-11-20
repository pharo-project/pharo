Class {
	#name : #MetacelloCommonVersionNumberTestCase,
	#superclass : #TestCase,
	#category : #'Metacello-TestsCore'
}

{ #category : #testing }
MetacelloCommonVersionNumberTestCase class >> isAbstract [
    "Override to true if a TestCase subclass is Abstract and should not have
	TestCase instances built from it"

    ^ self name = #'MetacelloCommonVersionNumberTestCase'
]

{ #category : #private }
MetacelloCommonVersionNumberTestCase >> defaultVersion1 [
  ^ self versionClass fromString: '1.0.0'
]

{ #category : #'test alpha/numeric version numbers' }
MetacelloCommonVersionNumberTestCase >> testAlphaNumericVersion1 [

	"Use numeric comparison for pure numbers. If you non-numeric version separate with '-'"
	
	| x y |
	self assert: ((x := self versionClass fromString: '2.9.0') < (y := self versionClass fromString: '2.10.0')).

]

{ #category : #'test alpha/numeric version numbers' }
MetacelloCommonVersionNumberTestCase >> testAlphaNumericVersion2 [

	self assert: ((self versionClass fromString: '2.9.0-alpha.2') < (self versionClass fromString: '2.9.0-alpha.3')).

]

{ #category : #'test alpha/numeric version numbers' }
MetacelloCommonVersionNumberTestCase >> testAlphaNumericVersion3 [

	self assert: ((self versionClass fromString: '2.9.9-alpha.2') < (self versionClass fromString: '2.9.10')).

]

{ #category : #'test alpha/numeric version numbers' }
MetacelloCommonVersionNumberTestCase >> testAlphaNumericVersion4 [

	self assert: ((self versionClass fromString: '2.9.9-alpha.2') < (self versionClass fromString: '2.9.9')).

]

{ #category : #'tests version increment/decrement' }
MetacelloCommonVersionNumberTestCase >> testIncrementDecrementVersion [
	| version |
	version := self defaultVersion1.
	self assert: version incrementMajorVersion asString equals: '2.0.0'.
	self assert: version decrementMajorVersion asString equals: '1.0.0'.
	self assert: version incrementMinorVersion asString equals: '1.1.0'.
	self assert: version decrementMinorVersion asString equals: '1.0.0'.
	self assert: version incrementPatchVersion asString equals: '1.0.1'.
	self assert: version decrementPatchVersion asString equals: '1.0.0'
]

{ #category : #'tests version increment/decrement' }
MetacelloCommonVersionNumberTestCase >> testIncrementDecrementVersionNumber [
	| version |
	version := self defaultVersion1.
	self assert: version incrementMinorVersionNumber asString equals: '1.0.1'.
	self assert: version decrementMinorVersionNumber asString equals: '1.0.0'
]

{ #category : #tests }
MetacelloCommonVersionNumberTestCase >> testVersion01 [
	self assert: (self versionClass fromString: '1.1.1') versionString equals: '1.1.1'
]

{ #category : #tests }
MetacelloCommonVersionNumberTestCase >> testVersion02 [
	| v1 v2 |
	v1 := self versionClass fromString: '1.1.1'.
	v2 := self versionClass fromString: '1.0.0'.
	self assert: v1 equals: v1.
	self assert: v2 equals: v2.
	self assert: v1 > v2
]

{ #category : #tests }
MetacelloCommonVersionNumberTestCase >> testVersion04 [

	| v1 v2 |
	v1 := self versionClass fromString: '1.0.1'.
	v2 := self versionClass fromString: '1.0.0'.
	self assert: (v1 > v2)
]

{ #category : #tests }
MetacelloCommonVersionNumberTestCase >> testVersion14 [

	self assert: ((self versionClass fromString: '2.9.0-alpha02') < (self versionClass fromString: '2.9.0-alpha03')).

]

{ #category : #tests }
MetacelloCommonVersionNumberTestCase >> testVersion22 [
	self assert: (MetacelloVersionNumber fromString: '3.0.0') collapseZeros size equals: 1.
	self assert: (MetacelloVersionNumber fromString: '3.0.0') equals: (MetacelloVersionNumber fromString: '3.0').
	self assert: (MetacelloVersionNumber fromString: '3.0') equals: (MetacelloVersionNumber fromString: '3.0.0')
]

{ #category : #tests }
MetacelloCommonVersionNumberTestCase >> testVersion23 [
	self assert: (MetacelloVersionNumber fromString: '3.0.0.-rc.1') equals: (MetacelloVersionNumber fromString: '3.0.0-rc.1').
	self assert: (MetacelloVersionNumber fromString: '3.0') > (MetacelloVersionNumber fromString: '3.0-rc.1').
	self assert: (MetacelloVersionNumber fromString: '3') > (MetacelloVersionNumber fromString: '3-rc.1').
	self assert: (MetacelloVersionNumber fromString: '3.-rc.1') equals: (MetacelloVersionNumber fromString: '3.0.0-rc.1').
	self assert: (MetacelloVersionNumber fromString: '3.0.-rc.1') equals: (MetacelloVersionNumber fromString: '3.0.0-rc.1').
	self assert: (MetacelloVersionNumber fromString: '3') > (MetacelloVersionNumber fromString: '3.0-rc.1').
	self assert: (MetacelloVersionNumber fromString: '3.0') > (MetacelloVersionNumber fromString: '3.0.0-rc.1')
]

{ #category : #tests }
MetacelloCommonVersionNumberTestCase >> testVersion25 [

	self assert: ((MetacelloVersionNumber fromString: '1.0-beta.24.0.1') < (MetacelloVersionNumber fromString: '1.0-beta.28')).

]

{ #category : #private }
MetacelloCommonVersionNumberTestCase >> versionClass [
    ^ self subclassResponsibility
]
