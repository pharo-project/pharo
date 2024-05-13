Class {
	#name : 'ShAbstractChangeDetectorTest',
	#superclass : 'TestCase',
	#instVars : [
		'newBuilder',
		'originClass',
		'usedTrait',
		'superClass'
	],
	#category : 'Shift-ClassBuilder-Tests-Changes',
	#package : 'Shift-ClassBuilder-Tests',
	#tag : 'Changes'
}

{ #category : 'testing' }
ShAbstractChangeDetectorTest class >> isAbstract [

	^ self == ShAbstractChangeDetectorTest
]

{ #category : 'running' }
ShAbstractChangeDetectorTest >> actualClass [

	^ self class environment at: (self class name withoutSuffix: #Test) asSymbol
]

{ #category : 'tests' }
ShAbstractChangeDetectorTest >> assertChangeAreDetected [

	self shouldnt: [ newBuilder compareWithOldClass ] raise: ShNoChangesInClass
]

{ #category : 'tests' }
ShAbstractChangeDetectorTest >> assertChangeArentDetected [

	self should: [ newBuilder compareWithOldClass ] raise: ShNoChangesInClass
]

{ #category : 'instance creation' }
ShAbstractChangeDetectorTest >> newComparer [

	^ self actualClass builder: newBuilder
]

{ #category : 'accessing' }
ShAbstractChangeDetectorTest >> packageName [

	^ #'Shift-Tests-Changes-Package-For-Test'
]

{ #category : 'running' }
ShAbstractChangeDetectorTest >> setUp [

	super setUp.

	usedTrait := self class classInstaller make: [ :builder |
		             builder
			             beTrait;
			             name: #TShTestForClass;
			             slots: #( tivar1 );
			             package: self packageName ].

	superClass := self class classInstaller make: [ :builder |
		              builder
			              name: #ShSuperTestForClass;
			              slots: { #superIvar1 };
			              package: self packageName ].

	originClass := self class classInstaller make: [ :builder |
		               builder
			               name: #ShTestForClass;
			               superclass: superClass;
			               traitComposition: usedTrait;
			               slots: { #ivar1 };
			               classSlots: { #classVar1 };
			               sharedVariables: { #Var1 };
			               sharedPools: { #TestSharedPool };
			               layoutClass: FixedLayout;
			               package: self packageName ].

	newBuilder := ShiftClassBuilder new
		              fillFor: originClass;
		              yourself
]

{ #category : 'running' }
ShAbstractChangeDetectorTest >> tearDown [

	{
		usedTrait.
		superClass.
		originClass } do: [ :class | class removeFromSystem ].

	self packageOrganizer removePackage: self packageName.

	super tearDown
]
