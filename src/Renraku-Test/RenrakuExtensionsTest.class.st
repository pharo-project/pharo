Class {
	#name : #RenrakuExtensionsTest,
	#superclass : #TestCase,
	#category : 'Renraku-Test'
}

{ #category : #tests }
RenrakuExtensionsTest >> testBanLevelCandidatesForClass [
	| class candidates |
	class := self class.
	candidates := class banLevelCandidates.
	self assert: (candidates includes: class).
	self assert: (candidates includes: class package).
	self assert: (candidates includes: ReImageRuleBaner).
	self assert: (candidates includes: RePersistentImageRuleBaner).	
]

{ #category : #tests }
RenrakuExtensionsTest >> testBanLevelCandidatesForMethod [
	| method candidates |
	method := thisContext  method.
	candidates := method banLevelCandidates.
	self assert: (candidates includes: method).
	self assert: (candidates includes: self class).
	self assert: (candidates includes: self class package).
	self assert: (candidates includes: ReImageRuleBaner).
	self assert: (candidates includes: RePersistentImageRuleBaner).	
]

{ #category : #tests }
RenrakuExtensionsTest >> testBanLevelCandidatesForPackage [
	| package candidates |
	package := self class package.
	candidates := package banLevelCandidates.
	self assert: (candidates includes: package).
	self assert: (candidates includes: ReImageRuleBaner).
	self assert: (candidates includes: RePersistentImageRuleBaner).	
]
