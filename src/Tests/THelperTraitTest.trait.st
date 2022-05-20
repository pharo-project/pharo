"
I am a Trait that add methods for classes and traits generation for testing traits.

Warning: Since this is a Trait, if the Trait are boggy then this trait will not work for Trait test.
"
Trait {
	#name : #THelperTraitTest,
	#instVars : [
		'trait1',
		'trait2',
		'trait3',
		'trait4',
		'testClassA',
		'testClassB',
		'testClassC',
		'testClassD'
	],
	#category : #'Tests-Traits-MOP'
}

{ #category : #'Method generation' }
THelperTraitTest classSide >> installMethodForClass: aClass [ 

	aClass compile: 'c

	^ ''C>>c'''
]

{ #category : #'method generation' }
THelperTraitTest classSide >> installMethodsForTrait1: aTrait [

	aTrait compile: 'c

	^ ''Trait1>>c'' '.

	aTrait compile: 'c1

	^ ''Trait1>>c1'' '
]

{ #category : #'Method generation' }
THelperTraitTest classSide >> installMethodsForTrait2: aTrait [ 
	
	aTrait compile: 'c

	^ ''Trait2>>c'' '.

	aTrait compile: 'c2

	^ ''Trait2>>c2'' '
]

{ #category : #'Method generation' }
THelperTraitTest classSide >> installMethodsForTrait3: aTrait [ 

	aTrait compile: 'c

	^ ''Trait3>>c'' '.

	aTrait compile: 'c3

	^ ''Trait3>>c3'' '
]

{ #category : #'Class generation' }
THelperTraitTest >> generateTestClassA [

	^ (Object << #MOPTestClassA)
		  traits: { trait3 };
		  slots: {  };
		  tag: 'Traits-MOP';
		  package: 'Tests';
		  install
]

{ #category : #'Class generation' }
THelperTraitTest >> generateTestClassB [

	^ (Object << #MOPTestClassB)
		  traits: { (trait1 + (trait2 - { #c })) };
		  slots: {  };
		  tag: 'Traits-MOP';
		  package: 'Tests';
		  install
]

{ #category : #'Class generation' }
THelperTraitTest >> generateTestClassC [

	^ (Object << #MOPTestClassC)
		  traits: { trait2 };
		  slots: {  };
		  tag: 'Traits-MOP';
		  package: 'Tests';
		  install
]

{ #category : #'Class generation' }
THelperTraitTest >> generateTestClassD [

	^ Object
		  subclass: #MOPTestClassD
		  uses: trait2 @ { (#c3 -> #c2) }
		  instanceVariableNames: ''
		  classVariableNames: ''
		  package: 'Tests-Traits-MOP'
]

{ #category : #'Class generation' }
THelperTraitTest >> generateTestClasses [

	testClassA := self generateTestClassA.

	testClassB := self generateTestClassB.

	testClassC := self generateTestClassC.
	self class installMethodForClass: testClassC.

	testClassD := self generateTestClassD
]

{ #category : #Generating }
THelperTraitTest >> generateTrait1 [

	^ (Trait << #Trait1)
		  tag: 'Traits-MOP';
		  package: 'Tests';
		  install
]

{ #category : #Generating }
THelperTraitTest >> generateTrait2 [

	^ (Trait << #Trait2)
		  tag: 'Traits-MOP';
		  package: 'Tests';
		  install
]

{ #category : #Generating }
THelperTraitTest >> generateTrait3 [

	^ (Trait << #Trait3)
		  traits: { trait2 };
		  tag: 'Traits-MOP';
		  package: 'Tests';
		  install
]

{ #category : #Generating }
THelperTraitTest >> generateTrait4 [

	^(Trait << #Trait4)
		          traits: { (trait1 + (trait2 - { #c })) };
		          tag: 'Traits-MOP';
		          package: 'Tests';
		          install
]

{ #category : #Generating }
THelperTraitTest >> generateTraitForTests [

	trait1 := self generateTrait1.
	self class installMethodsForTrait1: trait1.

	trait2 := self generateTrait2.
	self class installMethodsForTrait2: trait2.

	trait3 := self generateTrait3.
	self class installMethodsForTrait3: trait3.

	trait4 := self generateTrait4
]

{ #category : #cleaning }
THelperTraitTest >> removeTestClasses [

	testClassA removeFromSystem.
	testClassB removeFromSystem.
	testClassC removeFromSystem.
	testClassD removeFromSystem.
]

{ #category : #cleanup }
THelperTraitTest >> removeTraitForTests [

	trait1 removeFromSystem.
	trait2 removeFromSystem.
	trait3 removeFromSystem.
	trait4 removeFromSystem.
]
