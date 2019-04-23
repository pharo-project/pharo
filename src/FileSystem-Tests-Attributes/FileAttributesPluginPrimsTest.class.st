"
FileAttributePluginPrimsTests mostly tests failure cases.  Functionality tests are contained in DiskFileAttributeTests and FileReferenceAttributeTests.
"
Class {
	#name : #FileAttributesPluginPrimsTest,
	#superclass : #TestCase,
	#instVars : [
		'primitives'
	],
	#category : #'FileSystem-Tests-Attributes-Tests'
}

{ #category : #running }
FileAttributesPluginPrimsTest >> setUp [

	super setUp.
	primitives := File.
]

{ #category : #tests }
FileAttributesPluginPrimsTest >> testExistsPathTooLong [

	| pathMax filename |
	pathMax := primitives primPathMax.
	filename := String new: pathMax * 2 withAll: $a.
	self 
		should: [ primitives exists: filename ]
		raise: IllegalFileName.
]

{ #category : #tests }
FileAttributesPluginPrimsTest >> testFileAttributeNumberOutOfRange [
	"Check that the appropriate Bad Argument error is raised if an invalid attribute number is supplied"

	self 
		should: [ primitives fileAttribute: 'ignored' number: 0 ]
		raise: PrimitiveFailed.
	self 
		should: [ primitives fileAttribute: 'ignore' number: primitives fileAttributeNumberMap size + 1 ]
		raise: PrimitiveFailed.
]

{ #category : #tests }
FileAttributesPluginPrimsTest >> testPrimCloseDirNil [
	"FileAttributesPluginPrims>>primClosedir: nil should raise a bad argument error"

	self
		should: [ primitives primClosedir: nil ]
		raise: PrimitiveFailed
		withExceptionDo: [ :exception | 
			self assert: exception selector equals: #'bad argument' ].

]

{ #category : #tests }
FileAttributesPluginPrimsTest >> testPrimCloseDirString [
	"FileAttributesPluginPrims>>primClosedir: only accepts a ByteArray, anything else should raise an error"

	self
		should: [ primitives primClosedir: 'not a buffer' ]
		raise: PrimitiveFailed
		withExceptionDo: [ :exception | 
			self assert: exception selector equals: #'bad argument' ].

]

{ #category : #tests }
FileAttributesPluginPrimsTest >> testPrimCloseDirWrongLength [
	"FileAttributesPluginPrims>>primClosedir: only accepts a ByteArray of size 'void *'"

	self
		should: [ primitives primClosedir: (ByteArray new: 1) ]
		raise: PrimitiveFailed
		withExceptionDo: [ :exception | 
			self assert: exception selector equals: #'bad argument' ].

]

{ #category : #tests }
FileAttributesPluginPrimsTest >> testPrimExistsNil [
	"FileAttributesPluginPrims>>primClosedir: nil should raise a bad argument error"

	self
		should: [ primitives primExists: nil ]
		raise: PrimitiveFailed
		withExceptionDo: [ :exception | 
			self assert: exception selector equals: #'bad argument' ].

]
