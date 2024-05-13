Class {
	#name : 'ShSuperclassChangeDetectorTest',
	#superclass : 'ShAbstractChangeDetectorTest',
	#category : 'Shift-ClassBuilder-Tests-Changes',
	#package : 'Shift-ClassBuilder-Tests',
	#tag : 'Changes'
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
