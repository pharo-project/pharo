Class {
	#name : #MCVersionTest,
	#superclass : #MCTestCase,
	#instVars : [
		'version',
		'visited'
	],
	#category : #'Monticello-Tests-Base'
}

{ #category : #asserting }
MCVersionTest >> assert: aSelector orders: sexpr as: array [
	| expected |
	expected := OrderedCollection new.
	version := self versionFromTree: sexpr.
	version perform: aSelector with: [ :ea | expected add: ea info name ].
	self assert: expected asArray equals: array
]

{ #category : #asserting }
MCVersionTest >> assert: aSelector orders: sexpr as: expected unresolved: unresolved [
	| missing |
	missing := OrderedCollection new.
	version := self versionFromTree: sexpr.
	version perform: aSelector with: [ :ea | visited add: ea info name ] with: [ :ea | missing add: ea name ].
	self assert: visited asArray equals: expected.
	self assert: missing asArray equals: unresolved
]

{ #category : #building }
MCVersionTest >> dependencyFromTree: sexpr [
	^ MCMockDependency fromTree: sexpr
]

{ #category : #running }
MCVersionTest >> setUp [
	super setUp.
	visited := OrderedCollection new.
]

{ #category : #tests }
MCVersionTest >> testAllAvailablePostOrder [
	self 
		assert: #allAvailableDependenciesDo: 
		orders: #(a ((b (d e)) c)) 
		as: #(d e b c)
]

{ #category : #tests }
MCVersionTest >> testAllMissing [
	self 
		assert: #allDependenciesDo: 
		orders: #(a ((b (d e)) (c missing))) 
		as: #(d e b)
]

{ #category : #tests }
MCVersionTest >> testAllUnresolved [
	self 
		assert: #allDependenciesDo:ifUnresolved: 
		orders: #(a ((b (d e)) (c missing)))
		as: #(d e b)
		unresolved: #(c)
]

{ #category : #tests }
MCVersionTest >> testDependencyOrder [
	self 
		assert: #allDependenciesDo: 
		orders: #(a (b c)) 
		as: #(b c)
]

{ #category : #tests }
MCVersionTest >> testPostOrder [
	self 
		assert: #allDependenciesDo: 
		orders: #(a ((b (d e)) c)) 
		as: #(d e b c)
]

{ #category : #tests }
MCVersionTest >> testWithAll [
	self 
		assert: #withAllDependenciesDo: 
		orders: #(a ((b (d e)) c)) 
		as: #(d e b c a)
]

{ #category : #tests }
MCVersionTest >> testWithAllMissing [
	self 
		assert: #withAllDependenciesDo: 
		orders: #(a ((b (d e)) (c missing))) 
		as: #(d e b a)
]

{ #category : #tests }
MCVersionTest >> testWithAllUnresolved [
	self 
		assert: #withAllDependenciesDo:ifUnresolved: 
		orders: #(a ((b (d e)) (c missing)))
		as: #(d e b a)
		unresolved: #(c)
]

{ #category : #building }
MCVersionTest >> versionFromTree: sexpr [
	^ (self dependencyFromTree: sexpr) resolve
]
