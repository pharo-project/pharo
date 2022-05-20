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
