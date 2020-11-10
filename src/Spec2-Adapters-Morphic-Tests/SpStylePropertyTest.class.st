Class {
	#name : #SpStylePropertyTest,
	#superclass : #TestCase,
	#category : #'Spec2-Adapters-Morphic-Tests'
}

{ #category : #tests }
SpStylePropertyTest >> testMergeWith [
	| p1 p2 p3 |
	
	p1 := SpStyleGeometry new
		extent: 20@20;
		yourself.
	p2 := SpStyleGeometry new
		extent: 40@40;
		yourself.
		
	p3 := p1, p2.
	
	self assert: p3 extent equals: 40@40.
	
]

{ #category : #tests }
SpStylePropertyTest >> testMergeWithFontDoNotRemovePredefinedFontWhenNoFontIsDefined [
	| p1 p2 p3 |
	
	p1 := SpStyleFont new
		predefinedFont: #default;
		yourself.
	p2 := SpStyleFont new
		name: nil;
		color: Color white;
		yourself.
		
	p3 := p1, p2.
	
	self deny: p3 predefinedFont isNil.
	self assert: p3 color equals: Color white
]

{ #category : #tests }
SpStylePropertyTest >> testMergeWithFontRemovePredefinedFont [
	| p1 p2 p3 |
	
	p1 := SpStyleFont new
		predefinedFont: #default;
		yourself.
	p2 := SpStyleFont new
		name: 'Source Code Pro';
		size: 10;
		yourself.
		
	p3 := p1, p2.
	
	self assert: p3 predefinedFont isNil.
	
]

{ #category : #tests }
SpStylePropertyTest >> testMergeWithPredefinedFontCanBeDecorated [
	| p1 p2 p3 |
	
	p1 := SpStyleFont new
		predefinedFont: #default;
		yourself.
	p2 := SpStyleFont new
		name: nil;
		color: Color white;
		size: 42;
		yourself.
	
	p3 := p1, p2.
	
	self deny: p3 predefinedFont isNil.
	self assert: p3 color equals: Color white.
	self assert: p3 definedFont pointSize equals: 42
]
