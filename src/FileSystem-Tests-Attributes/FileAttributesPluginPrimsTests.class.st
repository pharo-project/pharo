"
FileAttributePluginPrimsTests mostly tests failure cases.  Functionality tests are contained in DiskFileAttributeTests and FileReferenceAttributeTests.
"
Class {
	#name : #FileAttributesPluginPrimsTests,
	#superclass : #TestCase,
	#instVars : [
		'primitives'
	],
	#category : #'FileSystem-Tests-Attributes'
}

{ #category : #running }
FileAttributesPluginPrimsTests >> setUp [

	super setUp.
	primitives := File.
]

{ #category : #tests }
FileAttributesPluginPrimsTests >> testExistsPathTooLong [

	| pathMax filename |
	pathMax := primitives primPathMax.
	filename := String new: pathMax * 2 withAll: $a.
	self 
		should: [ primitives exists: filename ]
		raise: IllegalName.
]

{ #category : #tests }
FileAttributesPluginPrimsTests >> testFileAttributeNumberOutOfRange [
	"Check that the appropriate Bad Argument error is raised if an invalid attribute number is supplied"

	self 
		should: [ primitives fileAttribute: 'ignored' number: 0 ]
		raise: PrimitiveFailed.
	self 
		should: [ primitives fileAttribute: 'ignore' number: primitives fileAttributeNumberMap size + 1 ]
		raise: PrimitiveFailed.
]

{ #category : #tests }
FileAttributesPluginPrimsTests >> testPrimCloseDirNil [
	"FileAttributesPluginPrims>>primClosedir: nil should raise a bad argument error"

	self
		should: [ primitives primClosedir: nil ]
		raise: PrimitiveFailed
		withExceptionDo: [ :exception | 
			self assert: exception selector equals: #primClosedir: ].

]

{ #category : #tests }
FileAttributesPluginPrimsTests >> testPrimCloseDirString [
	"FileAttributesPluginPrims>>primClosedir: only accepts a ByteArray, anything else should raise an error"

	self
		should: [ primitives primClosedir: 'a string' ]
		raise: PrimitiveFailed
		withExceptionDo: [ :exception | 
			self assert: exception selector equals: #primClosedir: ].

]

{ #category : #tests }
FileAttributesPluginPrimsTests >> testPrimCloseDirWrongLength [
	"FileAttributesPluginPrims>>primClosedir: only accepts a ByteArray of size 'void *'"

	self
		should: [ primitives primClosedir: (ByteArray new: 1) ]
		raise: PrimitiveFailed
		withExceptionDo: [ :exception | 
			self assert: exception selector equals: #primClosedir: ].

]

{ #category : #tests }
FileAttributesPluginPrimsTests >> testPrimExistsNil [
	"FileAttributesPluginPrims>>primClosedir: nil should raise a bad argument error"

	self
		should: [ primitives primExists: nil ]
		raise: PrimitiveFailed
		withExceptionDo: [ :exception | 
			self assert: exception selector equals: #primExists: ].

]
