Class {
	#name : #MCScannerTest,
	#superclass : #MCTestCase,
	#category : #'Monticello-Tests-Base'
}

{ #category : #asserting }
MCScannerTest >> assertScans: anArray [
	self assert: (MCScanner scan: anArray printString readStream) equals: anArray
]

{ #category : #tests }
MCScannerTest >> test1 [
	self assertScans: #(a '23' (x))
]

{ #category : #tests }
MCScannerTest >> test2 [
	self assertScans: 'it''s alive'
]

{ #category : #tests }
MCScannerTest >> test3 [
	self assert: (MCScanner scan: '(a #b c)' readStream) equals: #(a #b c)
]

{ #category : #tests }
MCScannerTest >> test4 [
	self assertScans: #(a '23' (x () ')''q' y12)).
]

{ #category : #tests }
MCScannerTest >> test5 [
	self assertScans: #((a) b)
]

{ #category : #tests }
MCScannerTest >> test6 [
	self should: [MCScanner scan: '(a b' readStream] raise: Error
]
