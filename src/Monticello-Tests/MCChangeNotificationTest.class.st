Class {
	#name : 'MCChangeNotificationTest',
	#superclass : 'MCTestCase',
	#instVars : [
		'workingCopy'
	],
	#category : 'Monticello-Tests-Base',
	#package : 'Monticello-Tests',
	#tag : 'Base'
}

{ #category : 'private' }
MCChangeNotificationTest >> foreignMethod [
	"see testForeignMethodModified"
]

{ #category : 'running' }
MCChangeNotificationTest >> setUp [
       "FIXME: Unregister Monticellomocks if it got created in another test
       (for example MCMethodDefinitionTest may create it implicitly).
       This avoids a nasty failure of MCChangeNotificationTest due to
       some inconsistency about the package info registered with it.
       If Monticellomocks was created earlier it will contain a 'regular'
       PackageInfo instance but the test requires it to be an MCMockPackageInfo"
"       MCWorkingCopy registry
               removeKey: (MCPackage new name: 'MonticelloMocks')
               ifAbsent:[].
       PackageOrganizer default unregisterPackageNamed: 'MonticelloMocks'."
		super setUp.	
      workingCopy := MCWorkingCopy ensureForPackage: self mockPackage.
]

{ #category : 'tests' }
MCChangeNotificationTest >> testCoreMethodModified [

	| method |
	workingCopy modified: false.
	method := self mockClassA compiledMethodAt: #one.
	MCWorkingCopy handleClassAndMethodsChange: (MethodModified methodChangedFrom: method to: method oldProtocol: nil).
	self assert: workingCopy modified
]

{ #category : 'tests' }
MCChangeNotificationTest >> testExtensionMethodModified [

	| method |
	workingCopy modified: false.
	method := workingCopy systemPackage extensionMethods first.
	MCWorkingCopy handleClassAndMethodsChange: (MethodModified methodChangedFrom: method to: method oldProtocol: nil).
	self assert: workingCopy modified
]

{ #category : 'tests' }
MCChangeNotificationTest >> testForeignMethodModified [

	| method |
	workingCopy modified: false.
	method := self class compiledMethodAt: #foreignMethod.
	MCWorkingCopy handleClassAndMethodsChange: (MethodModified methodChangedFrom: method to: method oldProtocol: nil).
	self deny: workingCopy modified
]
