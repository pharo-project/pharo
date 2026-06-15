"
I am a class to tests the trait composition detector of the shift class builder. 

I ensure that I detect a change if we update a trait composition and that I do not find any change if the trait composition stay the same
"
Class {
	#name : 'TraitCompositionChangedDetectorTest',
	#superclass : 'ShAbstractChangeDetectorTest',
	#category : 'Traits-Tests',
	#package : 'Traits-Tests'
}

{ #category : 'tests' }
TraitCompositionChangedDetectorTest >> testChangeInTraitCompositionIsDetected [

	| newTrait |
	newBuilder := ShiftClassBuilder new
		              fillFor: usedTrait;
		              yourself.
	[
		newTrait := self class classInstaller make: [ :builder |
				            builder
					            beTrait;
					            name: #TToUseForChangeComparison;
					            package: self packageName ].
		newBuilder traits: newTrait.

		self denyEmpty: self newComparer compareClass.
		self assertChangeAreDetected ] ensure: [ newTrait ifNotNil: [ newTrait removeFromSystem ] ]
]

{ #category : 'tests' }
TraitCompositionChangedDetectorTest >> testNoChangeInTraitCompositionIsDetected [

	newBuilder := ShiftClassBuilder new
		              fillFor: usedTrait;
		              yourself.
	newBuilder traits: {  }.

	self assertEmpty: self newComparer compareClass.
	self assertChangeArentDetected
]
