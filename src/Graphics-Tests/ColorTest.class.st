"
SUnit tests for class Color
"
Class {
	#name : #ColorTest,
	#superclass : #ClassTestCase,
	#category : #'Graphics-Tests-Primitives'
}

{ #category : #asserting }
ColorTest >> assertColorrefFor: aColor equals: anInteger [
	| colorref |
	colorref := aColor asColorref.
	self
		assert: colorref = anInteger
		description:
			('(<1p>) asColorref was <2s>; should have been <3s>' expandMacrosWith: aColor with: colorref hex with: anInteger hex)
]

{ #category : #coverage }
ColorTest >> classToBeTested [

	^Color
]

{ #category : #'tests - 32 bit' }
ColorTest >> test32BitBlackColorTranformation [
	"Solve bug: https://pharo.manuscript.com/f/cases/14619"

	self assert: ((Color black pixelWordForDepth: 32) asColorOfDepth: 32) equals: Color black
]

{ #category : #'tests - 32 bit' }
ColorTest >> test32BitOpaqueBlackIsTotallyBlack [
	"The pixel value of a black at depth32 should really be black..."
	"At the time of this test, it returned 16rFF000001 ...."
	self assert: 16rFF000000 equals: (Color black pixelValueForDepth: 32)
]

{ #category : #'tests - 32 bit' }
ColorTest >> test32BitTranslucentPixelValueKeepsRGB [
	"The pixel value of a translucent color at depth32 should keep the RGB component irrespective of alpha.
	At the time of this test, setting an alpha of zero made the entire pixel 0 irrespective of depth..."
	self assert: 16rFFFFFF equals: ((Color white alpha: 0) pixelValueForDepth: 32)
]

{ #category : #tests }
ColorTest >> testAsColorrefPrimaryColors [
"issue 13784 Color>>#asColorref ignores blue"
	self assertColorrefFor: Color black equals: 16r000000;
	assertColorrefFor: Color red equals: 16r0000FF;
	assertColorrefFor: Color green equals: 16r00FF00;
	assertColorrefFor: Color yellow equals: 16r00FFFF;
	assertColorrefFor: Color blue equals: 16rFF0000;
	assertColorrefFor: Color cyan equals: 16rFFFF00;
	assertColorrefFor: Color magenta  equals: 16rFF00FF;
	assertColorrefFor: Color white equals: 16rFFFFFF
]

{ #category : #tests }
ColorTest >> testAsHexString [
	| table aColorString |
	self assert: Color white asHexString equals: 'FFFFFF'.
	self assert: Color red asHexString equals: 'FF0000'.
	self assert: Color black asHexString equals: '000000'.

	table := #('0' '1' '2' '3' '4' '5' '6' '7' '8' '9' 'A' 'B' 'C' 'D' 'E' 'F').

	table
		do: [ :each | 
			aColorString := each , each , '0000'.
			self assert: ((Color fromHexString: aColorString) asHexString sameAs: aColorString) ].

	table
		do: [ :each | 
			aColorString := '00' , each , each , '00'.
			self assert: ((Color fromHexString: aColorString) asHexString sameAs: aColorString) ].

	table
		do: [ :each | 
			aColorString := '0000' , each , each.
			self assert: ((Color fromHexString: aColorString) asHexString sameAs: aColorString) ].

	table
		do: [ :each | 
			aColorString := each , each , each , each , each , each.
			self assert: ((Color fromHexString: aColorString) asHexString sameAs: aColorString) ]
]

{ #category : #tests }
ColorTest >> testColorFrom [
	self assert: ((Color colorFrom: #white) asHexString sameAs: 'ffffff').
	self assert: ((Color colorFrom: #(1.0 0.5 0.0)) asHexString sameAs: 'ff8000').
	self assert: ((Color colorFrom: (Color white)) asHexString sameAs: 'ffffff').
	self assert: ((Color colorFrom: '#FF8800') asHexString sameAs: 'ff8800').
	self assert: ((Color colorFrom: '#222222') asHexString sameAs: '222222').
]

{ #category : #tests }
ColorTest >> testFromHexString [

	| color |
	
	color := Color fromHexString: 'FFFFFF'.
	self assert: color equals: Color white.

	color := Color fromHexString: 'ff0000'.
	self assert: color equals: Color red.

	color := Color fromHexString: '#ff0000'.
	self assert: color equals: Color red.

	color := Color fromHexString: '#FF00FFFF'.
	self assert: color equals: Color magenta.

	color := Color fromHexString: '#FF00FF55'.
	self assert: (color alpha > 0.3 and: [color alpha < 0.4])

]

{ #category : #tests }
ColorTest >> testFromString [
	self assert: ((Color fromString: '#FF8800') asHexString sameAs: 'ff8800');
		assert: ((Color fromString: 'FF8800') asHexString sameAs: 'ff8800');
		assert: ((Color fromString: 'white') asHexString sameAs: 'ffffff');
		assert: ((Color fromString: 'black') asHexString sameAs: '000000');
		assert: ((Color fromString: nil) asHexString sameAs: 'ffffff');
		assert: ((Color fromString: 'inexistent color') asHexString sameAs: 'ffffff'); "should return white"
		assert: ((Color fromString: 'XXXXXX') asHexString sameAs: 'ffffff'); "not alphanumeric"
		assert: ((Color fromString: '00000000') asHexString sameAs: 'ffffff'). "too many digits"
		
	self
		assert: (Color fromString: 'DARKGRAY') = Color darkGray description: 'Color can be specified with a case insensitive color name';
		assert: (Color fromString: '#blue') = Color blue description: 'Color can be specified with a leading literal sharp'.
]

{ #category : #'tests - multiply' }
ColorTest >> testMultiplyByArray [
	| newColor oldColor tolerance |
	tolerance := 0.001.

	oldColor := Color r: 0.75 g: 0.5 b: 0.25.
	newColor := oldColor * #(0.1 2 3).

	self assert: (0.075 - newColor red) abs < tolerance.
	self assert: (1 - newColor green) abs < tolerance.
	self assert: (0.75 - newColor blue) abs < tolerance.
]

{ #category : #'tests - multiply' }
ColorTest >> testMultiplyByArrayIdentityTransform [
	| newColor oldColor tolerance |
	tolerance := 0.001.

	oldColor := Color r: 0.75 g: 0.5 b: 0.25.
	newColor := oldColor * 2.

	self assert: (1 - newColor red) abs < tolerance.
	self assert: (1 - newColor green) abs < tolerance.
	self assert: (0.5 - newColor blue) abs < tolerance.
]

{ #category : #'tests - multiply' }
ColorTest >> testMultiplyByNumber [
	| newColor oldColor tolerance |
	tolerance := 0.001.

	oldColor := Color r: 0.75 g: 0.5 b: 0.25.
	newColor := oldColor * 2.

	self assert: (1 - newColor red) abs < tolerance.
	self assert: (1 - newColor green) abs < tolerance.
	self assert: (0.5 - newColor blue) abs < tolerance.
]
