Class {
	#name : #OCCleanBlockTest,
	#superclass : #TestCase,
	#instVars : [
		'instVar'
	],
	#category : #'OpalCompiler-Tests-AST'
}

{ #category : #tests }
OCCleanBlockTest >> testBlockIsClean [
	self assert: [  ] sourceNode isClean.
	self assert: [ 1 + 2 ] sourceNode isClean.
	self assert: [ :a | a + 2 ] sourceNode isClean.
	self assert: [ :a :b | a + b + 3 ] sourceNode isClean.
	self assert: [ | a | a := 1. a + 3 ] sourceNode isClean.
	 
]

{ #category : #tests }
OCCleanBlockTest >> testBlockIsNotClean [
	| escpWrite escpRead |
	escpRead := escpWrite := 1.
	self deny: [ self foo ] sourceNode isClean.
	self deny: [ ^ 1 ] sourceNode isClean.
	self deny: [ instVar foo ] sourceNode isClean.
	self deny: [ escpRead foo ] sourceNode isClean.
	self deny: [ escpWrite := 2 ] sourceNode isClean.
	 
	self deny: [[ self foo ]] sourceNode isClean.
	self deny: [[ ^ 1 ]] sourceNode isClean.
	self deny: [[ instVar foo ]] sourceNode isClean.
	self deny: [[ escpRead foo ]] sourceNode isClean.
	self deny: [[ escpWrite := 2 ]] sourceNode isClean.
]
