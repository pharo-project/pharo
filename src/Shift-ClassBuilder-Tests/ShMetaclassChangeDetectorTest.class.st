Class {
	#name : 'ShMetaclassChangeDetectorTest',
	#superclass : 'ShAbstractChangeDetectorTest',
	#category : 'Shift-ClassBuilder-Tests-Changes',
	#package : 'Shift-ClassBuilder-Tests',
	#tag : 'Changes'
}

{ #category : 'tests' }
ShMetaclassChangeDetectorTest >> testChangeInMetaclassIsDetected [

	originClass := self class classInstaller make: [ :builder |
		               builder
			               name: #ShTestForClass;
			               superclass: superClass;
			               package: self packageName ].

	newBuilder := ShiftClassBuilder new
		              fillFor: originClass;
		              yourself.

	newBuilder
		traitComposition: usedTrait;
		createMetaclass.

	self denyEmpty: self newComparer compareClass.
	self assertChangeAreDetected
]

{ #category : 'tests' }
ShMetaclassChangeDetectorTest >> testNoChangeOfMetaclassDetected [

	newBuilder traitComposition: usedTrait.

	self assertEmpty: self newComparer compareClass.
	self assertChangeArentDetected
]
