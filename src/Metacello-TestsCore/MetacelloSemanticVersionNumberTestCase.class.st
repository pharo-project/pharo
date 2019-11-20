Class {
	#name : #MetacelloSemanticVersionNumberTestCase,
	#superclass : #MetacelloCommonVersionNumberTestCase,
	#category : #'Metacello-TestsCore'
}

{ #category : #tests }
MetacelloSemanticVersionNumberTestCase >> sampleVersionStrings [
    ^ #('1.0.0+-' '1.0.0-alpha' '1.0.0-alpha.1' '1.0.0-0.3.7' '1.0.0-x.7.z.92')
        , #('1.0.0+build.1' '1.3.7+build.11.e0f985a')
        ,
            #('1.0.0-alpha' '1.0.0-alpha.1' '1.0.0-beta.2' '1.0.0-beta.11' '1.0.0-rc.1' '1.0.0-rc.1+build.1' '1.0.0' '1.0.0+0.3.7' '1.3.7+build' '1.3.7+build.2.b8f12d7' '1.3.7+build.11.e0f985a')
        , #('1.0.0-alp-h-a' '1.0.0-r-c.1' '1.0.0+alp-h-a' '1.0.0+r-c.1')
]

{ #category : #tests }
MetacelloSemanticVersionNumberTestCase >> testEqualityComparison [
	self deny: '1.0.0+-' asMetacelloSemanticVersionNumber equals: '1.0.0--' asMetacelloSemanticVersionNumber.
	self sampleVersionStrings
		do: [ :versionString | self assert: versionString asMetacelloSemanticVersionNumber equals: versionString asMetacelloSemanticVersionNumber ]
]

{ #category : #'test matching' }
MetacelloSemanticVersionNumberTestCase >> testInvalidSpecialMatch [
    self should: [ (self versionClass fromString: '1.1.1') match: '?.?.?' ] raise: Error
]

{ #category : #tests }
MetacelloSemanticVersionNumberTestCase >> testIssue392 [
  "semantic version strings that are actually invalid, but were allowed by MetacelloOldSemanticVersionNumber"

  #('v0.0.1' '00.0.1' '16r00.0.1' '101r.0.1' 'rr101r.0.1')
    do: [ :versionString | 
      MetacelloOldSemanticVersionNumber fromString: versionString.
      self should: [ self versionClass fromString: versionString ] raise: Error ]
]

{ #category : #tests }
MetacelloSemanticVersionNumberTestCase >> testLessThanComparison [
    self assert: '1.0.0-rc.1+build.1' asMetacelloSemanticVersionNumber < '1.0.0' asMetacelloSemanticVersionNumber.
    self
        assert: '1.0.0-rc.1+build.1' asMetacelloSemanticVersionNumber < '1.0.0+build.0' asMetacelloSemanticVersionNumber.
    self assert: '1.0.0-alpha.1' asMetacelloSemanticVersionNumber < '1.0.0-0.3.7' asMetacelloSemanticVersionNumber.
    self assert: '1.0.0-alpha' asMetacelloSemanticVersionNumber < '1.0.0-alpha.1' asMetacelloSemanticVersionNumber.
    self assert: '1.0.0+-' asMetacelloSemanticVersionNumber < '1.0.0+a' asMetacelloSemanticVersionNumber.
    self assert: '1.0.0-0.3.7' asMetacelloSemanticVersionNumber < '1.0.0-x.7.z.92' asMetacelloSemanticVersionNumber
]

{ #category : #tests }
MetacelloSemanticVersionNumberTestCase >> testPrinting [
	| x vrsn |
	self sampleVersionStrings
		do: [ :versionString | self assert: versionString equals: (x := (vrsn := versionString asMetacelloSemanticVersionNumber) printString) ]
]

{ #category : #tests }
MetacelloSemanticVersionNumberTestCase >> testSemanticVersioning [
    self validateSemanticVersionStrings: #('1.0.0-rc.1+build.1' '1.0.0-rc.1+build.2' '1.0.0-rc.2+build.0').
    self validateSemanticVersionStrings: #('1.0.0-rc.1+build.1' '1.0.0+build.0').
    self validateSemanticVersionStrings: #('1.0.0-alpha' '1.0.0-1').
    self validateSemanticVersionStrings: #('1.0.0-1' '1.0.0+alpha').
    self validateSemanticVersionStrings: #('1.0.0-alpha' '1.0.0+1').
    self validateSemanticVersionStrings: #('1.0.0+alpha' '1.0.0+1').
    self validateSemanticVersionStrings: #('1.0.0--' '1.0.0-a').
    self validateSemanticVersionStrings: #('1.0.0+-' '1.0.0+a').
    self validateSemanticVersionStrings: #('1.0.0-rc.1' '1.0.0' '1.0.0+build.1').
    self validateSemanticVersionStrings: #('1.0.0-rc.1+build.1' '1.0.0-rc.1+build.2' '1.0.0-rc.2+build.3').
    self validateSemanticVersionStrings: #('1.0.0-rc.1' '1.0.0-rc.1+build.2' '1.0.0-rc.2').
    self validateSemanticVersionStrings: #('1.0.0-rc.1+build.1' '1.0.0-rc.1+build.2' '1.0.0-rc.2').
    self validateSemanticVersionStrings: #('1.0.0-rc.1+build.1' '1.0.0-rc.1+build.2' '1.0.0-rc.2+build.2').
    self validateSemanticVersionStrings: #('1.0.0-rc.1+build.1' '1.0.0' '1.0.0+build.0').
    self validateSemanticVersionStrings: #('1.0.0+10000' '1.0.0+a').
    self validateSemanticVersionStrings: #('1.0.0-10000' '1.0.0-a')
]

{ #category : #tests }
MetacelloSemanticVersionNumberTestCase >> testSemanticVersioningSpecItem10 [
    "[Semantic Versioning 2.0.0-rc.1](http://semver.org/)"

    self validateSemanticVersionStrings: #('1.0.0-alpha' '1.0.0-alpha.1' '1.0.0-0.3.7' '1.0.0-x.7.z.92')
]

{ #category : #tests }
MetacelloSemanticVersionNumberTestCase >> testSemanticVersioningSpecItem11 [
    "[Semantic Versioning 2.0.0-rc.1](http://semver.org/)"

    self validateSemanticVersionStrings: #('1.0.0+build.1' '1.3.7+build.11.e0f985a')
]

{ #category : #tests }
MetacelloSemanticVersionNumberTestCase >> testSemanticVersioningSpecItem12 [
    "According to [Semantic Versioning 2.0.0-rc.1](http://semver.org/), the following expression should be true:
	1.0.0-alpha < 1.0.0-alpha.1 < 1.0.0-beta.2 < 1.0.0-beta.11 < 1.0.0-rc.1 < 1.0.0-rc.1+build.1 < 1.0.0 < 
		1.0.0+0.3.7 < 1.3.7+build < 1.3.7+build.2.b8f12d7 < 1.3.7+build.11.e0f985a"

    self
        validateSemanticVersionStrings:
            #('1.0.0-alpha' '1.0.0-alpha.1' '1.0.0-beta.2' '1.0.0-beta.11' '1.0.0-rc.1' '1.0.0-rc.1+build.1' '1.0.0' '1.0.0+0.3.7' '1.3.7+build' '1.3.7+build.2.b8f12d7' '1.3.7+build.11.e0f985a')
]

{ #category : #tests }
MetacelloSemanticVersionNumberTestCase >> testSemanticVersioningSubsetCompliance [
    "subset of sample versions that are compatible with MetacellVersionNumber syntax"

    self
        validateSemanticVersionStrings:
            #('1.0.0-alpha' '1.0.0-alpha.1' '1.0.0-beta.2' '1.0.0-beta.11' '1.0.0-rc.1' '1.0.0')
]

{ #category : #'test matching' }
MetacelloSemanticVersionNumberTestCase >> testSpecialMatch [
    self assert: ((self versionClass fromString: '1.1.0') match: '?').
    self assert: ((self versionClass fromString: '1.0.0-alpha') match: '?').
    self assert: ((self versionClass fromString: '1.0.0-rc.1+build.1') match: '?').
    self assert: ((self versionClass fromString: '1.1.0') match: '*.*.?').
    self assert: ((self versionClass fromString: '1.3.7+build.2.b8f12d7') match: '?').
    self assert: ((self versionClass fromString: '1.3.7+build.11.e0f985a') match: '?').
    self deny: ((self versionClass fromString: '1.1.0') match: '*.*.*-?').
    self deny: ((self versionClass fromString: '1.1.0') match: '*.*.*+?').
    self assert: ((self versionClass fromString: '1.1.1') match: '*.?')
]

{ #category : #'test matching' }
MetacelloSemanticVersionNumberTestCase >> testStandardMatch [
    self assert: ((self versionClass fromString: '1.0.0+alpha5.0') match: '1.0.0+alpha#.0').
    self assert: ((self versionClass fromString: '1.1.1') match: '*.*.*').
    self deny: ((self versionClass fromString: '1.1.1') match: '*.*').
    self assert: ((self versionClass fromString: '1.1.0') match: '1.1.*').
    self assert: ((self versionClass fromString: '1.1.0') match: '1.#.*').
    self deny: ((self versionClass fromString: '1.10.0+build.0') match: '1.#.*').
    self assert: ((self versionClass fromString: '1.0.0-alpha5.0') match: '1.0.0-alpha#.0').
    self assert: ((self versionClass fromString: '1.0.0-alpha5+build5.0') match: '1.0.0-alpha5+build#.0').
    self deny: ((self versionClass fromString: '1.0.0-alpha10.0') match: '1.0.0-alpha#.0')
]

{ #category : #tests }
MetacelloSemanticVersionNumberTestCase >> testVersion05 [
    | v1 v2 |
    v1 := self versionClass fromString: '3.0.0'.
    v2 := self versionClass fromString: '2.0.0'.
    self assert: v1 > v2
]

{ #category : #tests }
MetacelloSemanticVersionNumberTestCase >> testVersion09 [
    | v1 v2 |
    v1 := self versionClass fromString: '1.0.0'.
    v2 := self versionClass fromString: '0.7.0'.
    self assert: v1 >= v2.
    self assert: v2 <= v1
]

{ #category : #tests }
MetacelloSemanticVersionNumberTestCase >> testVersion10 [
	| x y |
	self
		assert:
			(x := (({(self versionClass fromString: '1.0.0') . (self versionClass fromString: '0.7.0') . (self versionClass fromString: '0.8.0').
			(self versionClass fromString: '0.9.0') . (self versionClass fromString: '1.0.1')} sort: [ :a :b | a <= b ]) collect: [ :each | each versionString ]) asArray)
		equals: (y := #('0.7.0' '0.8.0' '0.9.0' '1.0.0' '1.0.1'))
]

{ #category : #tests }
MetacelloSemanticVersionNumberTestCase >> testVersion12 [
    self deny: (self versionClass fromString: '1.0.0') <= (self versionClass fromString: '0.7.0')
]

{ #category : #tests }
MetacelloSemanticVersionNumberTestCase >> testVersion15 [
    self assert: (self versionClass fromString: '1.0.0-beta.0') < (self versionClass fromString: '1.0.0-beta.1')
]

{ #category : #tests }
MetacelloSemanticVersionNumberTestCase >> testVersion16 [
    self assert: (self versionClass fromString: '1.0.0-beta.0') < (self versionClass fromString: '1.0.0')
]

{ #category : #tests }
MetacelloSemanticVersionNumberTestCase >> testVersion17 [
    self assert: (self versionClass fromString: '1.0.0') > (self versionClass fromString: '1.0.0-0').
    self assert: (self versionClass fromString: '1.0.0') > (self versionClass fromString: '1.0.0-beta.0').
    self assert: (self versionClass fromString: '1.0.0') > (self versionClass fromString: '1.0.0-beta')
]

{ #category : #tests }
MetacelloSemanticVersionNumberTestCase >> testVersion18 [
    | x y |
    self deny: (x := self versionClass fromString: '1.0.0') < (y := self versionClass fromString: '1.0.0-0').
    self assert: (x := self versionClass fromString: '1.0.0') > (y := self versionClass fromString: '1.0.0-0').
    self assert: (x := self versionClass fromString: '1.0.0') < (y := self versionClass fromString: '1.0.0+0').
    self deny: (x := self versionClass fromString: '1.0.0') > (y := self versionClass fromString: '1.0.0+0')
]

{ #category : #tests }
MetacelloSemanticVersionNumberTestCase >> testVersion19 [
    self assert: (self versionClass fromString: '1.0.0-beta.0') < (self versionClass fromString: '1.0.0')
]

{ #category : #tests }
MetacelloSemanticVersionNumberTestCase >> validateSemanticVersionStrings: versionStrings [
    | versions version |
    versions := versionStrings collect: [ :each | each asMetacelloSemanticVersionNumber ].
    version := versions at: 1.
    2 to: versions size do: [ :index | 
        | nextVersion |
        nextVersion := versions at: index.
        self assert: version < nextVersion.
        version := nextVersion ]
]

{ #category : #private }
MetacelloSemanticVersionNumberTestCase >> versionClass [
    ^ MetacelloSemanticVersionNumber
]
