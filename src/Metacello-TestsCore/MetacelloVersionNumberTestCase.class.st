Class {
	#name : #MetacelloVersionNumberTestCase,
	#superclass : #MetacelloCommonVersionNumberTestCase,
	#category : #'Metacello-TestsCore'
}

{ #category : #'test approximately greater than' }
MetacelloVersionNumberTestCase >> testApproxVersion01 [

	self assert: '1.1.1' asMetacelloVersionNumber ~> '1.1' asMetacelloVersionNumber
]

{ #category : #'test approximately greater than' }
MetacelloVersionNumberTestCase >> testApproxVersion02 [

	self deny: '1.1' asMetacelloVersionNumber ~> '1' asMetacelloVersionNumber
]

{ #category : #'test approximately greater than' }
MetacelloVersionNumberTestCase >> testApproxVersion03 [

	self assert: '1.1' asMetacelloVersionNumber ~> '1.1' asMetacelloVersionNumber
]

{ #category : #tests }
MetacelloVersionNumberTestCase >> testCollapseZeros [
	self assert: (MetacelloVersionNumber fromString: '1.0-beta.24.0.1') collapseZeros printString equals: '1-beta.24.0.1'.
	self assert: (MetacelloVersionNumber fromString: '1.0-beta.24.0.0.1') collapseZeros printString equals: '1-beta.24.0.0.1'.
	self assert: (MetacelloVersionNumber fromString: '1.0.0-beta.24.0.0.1') collapseZeros printString equals: '1-beta.24.0.0.1'
]

{ #category : #'tests version increment/decrement' }
MetacelloVersionNumberTestCase >> testIncrementDecrementVersionNumber [
	| version |
	super testIncrementDecrementVersionNumber.
	version := '1' asMetacelloVersionNumber.
	self assert: version incrementMinorVersionNumber asString equals: '2'.
	self assert: version decrementMinorVersionNumber asString equals: '1'.
	version := '1.0' asMetacelloVersionNumber.
	self assert: version incrementMinorVersionNumber asString equals: '1.1'.
	self assert: version decrementMinorVersionNumber asString equals: '1.0'.
	version := '1.0.0' asMetacelloVersionNumber.
	self assert: version incrementMinorVersionNumber asString equals: '1.0.1'.
	self assert: version decrementMinorVersionNumber asString equals: '1.0.0'.
	version := '1.0.0.0' asMetacelloVersionNumber.
	self assert: version incrementMinorVersionNumber asString equals: '1.0.0.1'.
	self assert: version decrementMinorVersionNumber asString equals: '1.0.0.0'
]

{ #category : #'test matching' }
MetacelloVersionNumberTestCase >> testInvalidSpecialMatch [

	self 
		should: [ ((self versionClass fromString: '1.1') match: '?.?') ]
		raise: Error.

]

{ #category : #'test matching' }
MetacelloVersionNumberTestCase >> testSpecialMatch [

	self assert: ((self versionClass fromString: '1.1') match: '?').
	self assert: ((self versionClass fromString: 'alpha') match: '?').
	self assert: ((self versionClass fromString: '1.1.1.1.1') match: '?').
	self assert: ((self versionClass fromString: '1.alpha5') match: '?').
	self assert: ((self versionClass fromString: '1.1') match: '*.?').
	self assert: ((self versionClass fromString: '1.1') match: '?').
	self assert: ((self versionClass fromString: '10.1.1.1.1') match: '?').
	self deny: ((self versionClass fromString: '1.1') match: '*.*.?').
	self assert: ((self versionClass fromString: '1.1.1') match: '*.?').

]

{ #category : #'test matching' }
MetacelloVersionNumberTestCase >> testStandardMatch [

	self assert: ((self versionClass fromString: '1.1.1') match: '*.*.*').
	self deny: ((self versionClass fromString: '1.1.1') match: '*.*').
	self assert: ((self versionClass fromString: '1.1.0') match: '1.1.*').
	self assert: ((self versionClass fromString: '1.1.0') match: '1.#.*').
	self deny: ((self versionClass fromString: '1.10.0') match: '1.#.*').
	self assert: ((self versionClass fromString: '1-alpha5.0') match: '1-alpha#.0').
	self assert: ((self versionClass fromString: '1.alpha5.0') match: '1-alpha#.0').
	self assert: ((self versionClass fromString: '1.alpha5.0') match: '1.alpha#.0').
	self deny: ((self versionClass fromString: '1-alpha10.0') match: '1-alpha#.0').

]

{ #category : #tests }
MetacelloVersionNumberTestCase >> testVersion03 [

	| v1 v2 |
	v1 := self versionClass fromString: '1.0.0.1'.
	v2 := self versionClass fromString: '1.0.0'.
	self assert: (v1 > v2)
]

{ #category : #tests }
MetacelloVersionNumberTestCase >> testVersion05 [

	| v1 v2 |
	v1 := self versionClass fromString: '3'.
	v2 := self versionClass fromString: '2'.
	self assert: (v1 > v2)
]

{ #category : #tests }
MetacelloVersionNumberTestCase >> testVersion06 [

	| v1 v2 |
	v1 := self versionClass fromString: '3.'.
	v2 := self versionClass fromString: '2'.
	self assert: (v1 > v2)
]

{ #category : #tests }
MetacelloVersionNumberTestCase >> testVersion07 [

	| v1 v2 |
	v1 := self versionClass fromString: '3.0.0'.
	v2 := self versionClass fromString: '2'.
	self assert: (v1 > v2)
]

{ #category : #tests }
MetacelloVersionNumberTestCase >> testVersion08 [

	| v1 v2 |
	v1 := self versionClass fromString: '1.0a6'.
	v2 := self versionClass fromString: '1.0a5'.
	self assert: (v1 > v2)
]

{ #category : #tests }
MetacelloVersionNumberTestCase >> testVersion09 [

	| v1 v2 |
	v1 := self versionClass fromString: '1.0'.
	v2 := self versionClass fromString: '0.7'.
	self assert: (v1 >= v2).
	self assert: (v2 <= v1)
]

{ #category : #tests }
MetacelloVersionNumberTestCase >> testVersion10 [
	| x y |
	self
		assert:
			(x := (({(self versionClass fromString: '1.0') . (self versionClass fromString: '0.7') . (self versionClass fromString: '0.8').
			(self versionClass fromString: '0.9') . (self versionClass fromString: '1.0.1')} sort: [ :a :b | a <= b ]) collect: [ :each | each versionString ]) asArray)
		equals: (y := #('0.7' '0.8' '0.9' '1.0' '1.0.1'))
]

{ #category : #tests }
MetacelloVersionNumberTestCase >> testVersion11 [

	| v1 v2 |
	v1 := self versionClass fromString: '1.0.1b'.
	v2 := self versionClass fromString: '1.0.1a'.
	self assert: (v1 >= v2).
	self assert: (v2 <= v1)
]

{ #category : #tests }
MetacelloVersionNumberTestCase >> testVersion12 [

	self deny: ((self versionClass fromString: '1.0') <= (self versionClass fromString: '0.7'))
]

{ #category : #tests }
MetacelloVersionNumberTestCase >> testVersion13 [

	self deny: ((self versionClass fromString: '0.8') <= (self versionClass fromString: '0.7')).
	self deny: ((self versionClass fromString: '0.8.1.8') <= (self versionClass fromString: '0.7.0.5')).
	
]

{ #category : #tests }
MetacelloVersionNumberTestCase >> testVersion15 [

	self assert: ((self versionClass fromString: '1.0-beta.0') < (self versionClass fromString: '1.0-beta.1')).

]

{ #category : #tests }
MetacelloVersionNumberTestCase >> testVersion16 [
    self assert: (self versionClass fromString: '1.0-beta.0') < (self versionClass fromString: '1.0').
    self assert: (self versionClass fromString: '1.0-beta.0') < (self versionClass fromString: '1.0.0').
    self assert: (self versionClass fromString: '1.0-beta.0') < (self versionClass fromString: '1.0.0.0')
]

{ #category : #tests }
MetacelloVersionNumberTestCase >> testVersion17 [
	self assert: (self versionClass fromString: '1.0') equals: (self versionClass fromString: '1.0.0').
	self assert: (self versionClass fromString: '1') equals: (self versionClass fromString: '1.0').
	self assert: (self versionClass fromString: '1') > (self versionClass fromString: '1-0').
	self assert: (self versionClass fromString: '1') > (self versionClass fromString: '1.0-beta.0').
	self assert: (self versionClass fromString: '1') > (self versionClass fromString: '1-beta.0').
	self assert: (self versionClass fromString: '1') > (self versionClass fromString: '1-beta')
]

{ #category : #tests }
MetacelloVersionNumberTestCase >> testVersion18 [

	self deny: ((self versionClass fromString: '1.0') < (self versionClass fromString: '1')).
	self deny: ((self versionClass fromString: '1.0') < (self versionClass fromString: '1-0')).

]

{ #category : #tests }
MetacelloVersionNumberTestCase >> testVersion19 [

	self assert: ((self versionClass fromString: '1.0-beta.0') < (self versionClass fromString: '1')).

]

{ #category : #tests }
MetacelloVersionNumberTestCase >> testVersion20 [
	self assert: (MetacelloVersionNumber fromString: '') printString equals: ''
]

{ #category : #tests }
MetacelloVersionNumberTestCase >> testVersion21 [
	self deny: (MetacelloVersionNumber fromString: '') > (MetacelloVersionNumber fromString: '0').
	self assert: (MetacelloVersionNumber fromString: '') < (MetacelloVersionNumber fromString: '0').
	self assert: (MetacelloVersionNumber fromString: '') equals: (MetacelloVersionNumber fromString: '')
]

{ #category : #tests }
MetacelloVersionNumberTestCase >> testVersion24 [
	self assert: (MetacelloVersionNumber fromString: '3.0.3.-rc.1') ~= (MetacelloVersionNumber fromString: '3.0.0.3-rc.1').
	self assert: (MetacelloVersionNumber fromString: '1.0.0.1.0.0') equals: (MetacelloVersionNumber fromString: '1.0.0.1').
	self assert: (MetacelloVersionNumber fromString: '1.0.0.1') ~= (MetacelloVersionNumber fromString: '1..1')
]

{ #category : #private }
MetacelloVersionNumberTestCase >> versionClass [

	^MetacelloVersionNumber
]
