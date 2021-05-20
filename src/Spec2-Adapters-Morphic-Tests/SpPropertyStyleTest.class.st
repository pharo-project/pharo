Class {
	#name : #SpPropertyStyleTest,
	#superclass : #TestCase,
	#category : #'Spec2-Adapters-Morphic-Tests'
}

{ #category : #tests }
SpPropertyStyleTest >> testMergeWith [
	| p1 p2 p3 |
	
	p1 := SpGeometryStyle new
		width: 20;
		height: 20;
		yourself.
	p2 := SpGeometryStyle new
		width: 40;
		height: 40;
		yourself.
		
	p3 := p1, p2.
	
	self assert: p3 width equals: 40.
	self assert: p3 height equals: 40	
]

{ #category : #tests }
SpPropertyStyleTest >> testMergeWithFontDoNotRemovePredefinedFontWhenNoFontIsDefined [
	| p1 p2 p3 |
	
	p1 := SpFontStyle new
		predefinedFont: #default;
		yourself.
	p2 := SpFontStyle new
		name: nil;
		color: Color white;
		yourself.
		
	p3 := p1, p2.
	
	self deny: p3 predefinedFont isNil.
	self assert: p3 color equals: Color white
]

{ #category : #tests }
SpPropertyStyleTest >> testMergeWithFontRemovePredefinedFont [
	| p1 p2 p3 |
	
	p1 := SpFontStyle new
		predefinedFont: #default;
		yourself.
	p2 := SpFontStyle new
		name: 'Source Code Pro';
		size: 10;
		yourself.
		
	p3 := p1, p2.
	
	self assert: p3 predefinedFont isNil.
	
]

{ #category : #tests }
SpPropertyStyleTest >> testMergeWithPredefinedFontCanBeDecorated [
	| p1 p2 p3 |
	
	p1 := SpFontStyle new
		predefinedFont: #default;
		yourself.
	p2 := SpFontStyle new
		name: nil;
		color: Color white;
		size: 42;
		yourself.
	
	p3 := p1, p2.
	
	self deny: p3 predefinedFont isNil.
	self assert: p3 color equals: Color white.
	self assert: p3 definedFont pointSize equals: 42
]
