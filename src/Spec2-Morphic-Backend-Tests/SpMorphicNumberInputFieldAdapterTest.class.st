"
I am a test class for the MorphicNumberInputFieldAdapter.
"
Class {
	#name : #SpMorphicNumberInputFieldAdapterTest,
	#superclass : #TestCase,
	#instVars : [
		'presenter',
		'adapter'
	],
	#category : #'Spec2-Morphic-Backend-Tests'
}

{ #category : #helpers }
SpMorphicNumberInputFieldAdapterTest >> actualClass [
	^ SpMorphicNumberInputFieldAdapter
]

{ #category : #running }
SpMorphicNumberInputFieldAdapterTest >> setUp [
	super setUp.
	presenter := SpNumberInputFieldPresenter new.
	adapter := self actualClass adapt: presenter
]

{ #category : #tests }
SpMorphicNumberInputFieldAdapterTest >> testDecreasedValueFrom [
	presenter
		beInteger;
		minimum: 80;
		climbRate: 2.
		
	self assert: (adapter decreasedValueFrom: '85') equals: '83'.
	self assert: (adapter decreasedValueFrom: '81') equals: '80'.
	self assert: (adapter decreasedValueFrom: '80') equals: '80'.
	
	presenter
		beFloat;
		climbRate: 2.5;
		digits: 1.

	self assert: (adapter decreasedValueFrom: '85') equals: '82.5'.
	self assert: (adapter decreasedValueFrom: '81') equals: '80.0'
]

{ #category : #tests }
SpMorphicNumberInputFieldAdapterTest >> testInBounds [
	presenter maximum: nil.
	presenter minimum: nil.
	self assert: (adapter inBounds: 100) equals: 100.
	self assert: (adapter inBounds: 160) equals: 160.
	self assert: (adapter inBounds: 80) equals: 80.
	presenter maximum: 150.
	presenter minimum: 90.
	self assert: (adapter inBounds: 100) equals: 100.
	self assert: (adapter inBounds: 160) equals: 150.
	self assert: (adapter inBounds: 80) equals: 90
]

{ #category : #tests }
SpMorphicNumberInputFieldAdapterTest >> testIncreasedValueFrom [
	presenter
		beInteger;
		maximum: 100;
		climbRate: 2.
		
	self assert: (adapter increasedValueFrom: '85') equals: '87'.
	self assert: (adapter increasedValueFrom: '99') equals: '100'.
	self assert: (adapter increasedValueFrom: '100') equals: '100'.
	
	presenter
		beFloat;
		climbRate: 2.5;
		digits: 1.

	self assert: (adapter increasedValueFrom: '85') equals: '87.5'.
	self assert: (adapter increasedValueFrom: '99') equals: '100.0'
]

{ #category : #tests }
SpMorphicNumberInputFieldAdapterTest >> testInferiorToMaximum [
	presenter maximum: nil.
	self assert: (adapter inferiorToMaximum: 100) equals: 100.
	presenter maximum: 150.
	self assert: (adapter inferiorToMaximum: 100) equals: 100.
	presenter maximum: 90.
	self assert: (adapter inferiorToMaximum: 100) equals: 90
]

{ #category : #tests }
SpMorphicNumberInputFieldAdapterTest >> testShouldAccept [
	presenter
		beInteger;
		minimum: 90;
		maximum: 120.

	self assert: (adapter shouldAccept: '').
	self assert: (adapter shouldAccept: '100').
	self deny: (adapter shouldAccept: 'string').
	self deny: (adapter shouldAccept: '10string')
]

{ #category : #tests }
SpMorphicNumberInputFieldAdapterTest >> testShouldAcceptFloat [
	presenter
		beFloat;
		minimum: 90;
		maximum: 120;
		digits: 3.

	self assert: (adapter shouldAccept: '').
	self assert: (adapter shouldAccept: '100').
	self assert: (adapter shouldAccept: '100.1').
	self deny: (adapter shouldAccept: '80.1').
	self deny: (adapter shouldAccept: '150.1').
	self deny: (adapter shouldAccept: 'string').
	self deny: (adapter shouldAccept: '10string').
	self deny: (adapter shouldAccept: '100.1111')
]

{ #category : #tests }
SpMorphicNumberInputFieldAdapterTest >> testShouldAcceptFloatWithMaxDigits [
	presenter
		beFloat;
		minimum: 0;
		maximum: 120;
		digits: 5.
		
	self assert: (adapter shouldAccept: '1.1111').
	self assert: (adapter shouldAccept: '1.11111').
	self deny: (adapter shouldAccept: '1.111111')
]

{ #category : #tests }
SpMorphicNumberInputFieldAdapterTest >> testShouldAcceptFloatWithoutMinMax [
	presenter
		beFloat;
		digits: 3;
		maximum: nil;
		minimum: nil.
		
	self assert: (adapter shouldAccept: '').
	self assert: (adapter shouldAccept: '100').
	self assert: (adapter shouldAccept: '100.1').
	self assert: (adapter shouldAccept: '80.1').
	self assert: (adapter shouldAccept: '150.1').
	self deny: (adapter shouldAccept: 'string').
	self deny: (adapter shouldAccept: '10string')
]

{ #category : #tests }
SpMorphicNumberInputFieldAdapterTest >> testShouldAcceptInteger [
	presenter
		beInteger;
		minimum: 90;
		maximum: 120.

	self assert: (adapter shouldAccept: '').
	self assert: (adapter shouldAccept: '100').
	self deny: (adapter shouldAccept: '100.10').
	self deny: (adapter shouldAccept: '80').
	self deny: (adapter shouldAccept: '150').
	self deny: (adapter shouldAccept: 'string').
	self deny: (adapter shouldAccept: '10string')
]

{ #category : #tests }
SpMorphicNumberInputFieldAdapterTest >> testShouldAcceptIntegerWithoutMinMax [
	presenter
		beInteger;
		maximum: nil;
		minimum: nil.
		
	self assert: (adapter shouldAccept: '').
	self assert: (adapter shouldAccept: '100').
	self deny: (adapter shouldAccept: '100.10').
	self assert: (adapter shouldAccept: '80').
	self assert: (adapter shouldAccept: '150').
	self deny: (adapter shouldAccept: 'string').
	self deny: (adapter shouldAccept: '10string')
]

{ #category : #tests }
SpMorphicNumberInputFieldAdapterTest >> testShouldAcceptNegativeFloat [
	presenter
		beFloat;
		maximum: 120;
		digits: 3;
		minimum: -150.
	
	self assert: (adapter shouldAccept: '-100.1').
	self deny: (adapter shouldAccept: '-200.1')
]

{ #category : #tests }
SpMorphicNumberInputFieldAdapterTest >> testShouldAcceptNegativeInteger [
	presenter
		beInteger;
		maximum: 120;
		minimum: -150.
	
	self assert: (adapter shouldAccept: '-100').
	self deny: (adapter shouldAccept: '-200')
]

{ #category : #tests }
SpMorphicNumberInputFieldAdapterTest >> testSuperiorToMinimum [
	presenter minimum: nil.
	self assert: (adapter superiorToMinimum: 100) equals: 100.
	presenter minimum: 90.
	self assert: (adapter superiorToMinimum: 100) equals: 100.
	presenter minimum: 120.
	self assert: (adapter superiorToMinimum: 100) equals: 120
]
