Class {
	#name : 'ShSuperclassChangeDetectorTest',
	#superclass : 'ShAbstractChangeDetectorTest',
	#category : 'Shift-Changes-Tests',
	#package : 'Shift-Changes-Tests'
}

{ #category : 'tests' }
ShSuperclassChangeDetectorTest >> testChangeInSuperclassIsDetected [

	| newSuperclass |
	[
	newSuperclass := self class classInstaller make: [ :builder |
		                 builder
			                 name: #ShSuperTestForClass2;
			                 package: self packageName ].
	newBuilder superclass: newSuperclass.

	self denyEmpty: self newComparer compareClass.
	self assertChangeAreDetected ] ensure: [ newSuperclass ifNotNil: [ newSuperclass removeFromSystem ] ]
]

{ #category : 'tests' }
ShSuperclassChangeDetectorTest >> testNoChangeInSuperclassDetected [

	newBuilder superclass: superClass.

	self assertEmpty: self newComparer compareClass.
	self assertChangeArentDetected
]
