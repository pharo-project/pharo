Class {
	#name : #ShClassInstallerTest,
	#superclass : #TestCase,
	#instVars : [
		'newClass',
		'superClass',
		'subClass'
	],
	#category : 'Shift-ClassInstaller-Tests'
}

{ #category : #'instance creation' }
ShClassInstallerTest >> newClass: className slots: slots [
	^ self newClass: className superclass: Object slots: slots.
]

{ #category : #'instance creation' }
ShClassInstallerTest >> newClass: className superclass: aSuperclass slots: slots [
	^ ShiftClassInstaller
		make: [ :builder | 
			builder
				name: className;
				superclass: aSuperclass;
				slots: slots;
				sharedVariables: '';
				sharedPools: '';
				category: 'Shift-ClassInstaller-Tests';
				copyClassSlotsFromExistingClass ]
]

{ #category : #running }
ShClassInstallerTest >> tearDown [
	newClass ifNotNil: #removeFromSystem.
	superClass ifNotNil: #removeFromSystem.
	subClass ifNotNil: #removeFromSystem.
	
	super tearDown.
]

{ #category : #tests }
ShClassInstallerTest >> testClassWithComment [
	newClass := ShiftClassInstaller
		make: [ :builder | 
			builder
				name: #SHClassWithComment;
				superclass: Object;
				slots: #();
				sharedVariables: '';
				sharedPools: '';
				category: 'Shift-ClassInstaller-Tests';
				comment: 'I have a comment'; 
				commentStamp: 'anStamp';				
				copyClassSlotsFromExistingClass ].
	
	self assert: newClass comment equals: 'I have a comment'.
	self assert: newClass organization commentStamp equals: 'anStamp'.
]

{ #category : #tests }
ShClassInstallerTest >> testModifyingSuperclass [
	superClass := self newClass:#ShCITestClass slots:#(anInstanceVariable).
	newClass := self newClass: #ShCITestSubClass superclass: superClass slots: #().
	
	superClass new instVarNamed: #anInstanceVariable.
	newClass new instVarNamed: #anInstanceVariable.	
	
	superClass := self newClass:#ShCITestClass slots:#(anInstanceVariable otherVariable).

	superClass new instVarNamed: #anInstanceVariable.
	newClass new instVarNamed: #anInstanceVariable.	
	superClass new instVarNamed: #otherVariable.
	newClass new instVarNamed: #otherVariable.	

]
