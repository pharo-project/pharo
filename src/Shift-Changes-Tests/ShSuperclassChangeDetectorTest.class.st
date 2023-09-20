Class {
	#name : 'ShSuperclassChangeDetectorTest',
	#superclass : 'ShAbstractChangeDetectorTest',
	#category : 'Shift-Changes-Tests',
	#package : 'Shift-Changes-Tests'
}

{ #category : 'tests' }
ShSuperclassChangeDetectorTest >> testChangeInSuperclassIsDetected [

	newBuilder superclass: (self class classInstaller make: [ :builder |
			 builder
				 name: #ShSuperTestForClass2;
				 package: self packageName ]).

	self denyEmpty: self newComparer compareClass.
	self assertChangeAreDetected
]

{ #category : 'tests' }
ShSuperclassChangeDetectorTest >> testNoChangeInSuperclassDetected [

	newBuilder superclass: superClass.

	self assertEmpty: self newComparer compareClass.
	self assertChangeArentDetected
]
