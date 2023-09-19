Class {
	#name : 'ShSuperclassChangedDetectorTest',
	#superclass : 'ShAbstractChangeDetectorTest',
	#category : 'Shift-Changes-Tests',
	#package : 'Shift-Changes-Tests'
}

{ #category : 'tests' }
ShSuperclassChangedDetectorTest >> testChangeInSuperclassIsDetected [

	newBuilder superclass: (self class classInstaller make: [ :builder |
			 builder
				 name: #ShSuperTestForClass2;
				 package: self packageName ]).

	self denyEmpty: self newComparer compareClass.
	self assertChangeAreDetected
]

{ #category : 'tests' }
ShSuperclassChangedDetectorTest >> testNoChangeInSuperclassDetected [

	newBuilder superclass: superClass.

	self assertEmpty: self newComparer compareClass.
	self assertChangeArentDetected
]
