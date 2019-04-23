Class {
	#name : #CDMetaclassParserTest,
	#superclass : #CDBehaviorParserTest,
	#category : #'ClassParser-Tests'
}

{ #category : #testing }
CDMetaclassParserTest class >> isAbstract [ 
	^ self == CDMetaclassParserTest
]

{ #category : #introspection }
CDMetaclassParserTest >> className [
	"Answer a string characterizing the receiver's class, for use in list views for example"

	^ self class name asString
]

{ #category : #tests }
CDMetaclassParserTest >> testClassDefFromLegacyStringHasClassNameNode [

	self assert: classDefinition classNameNode value equals: self className.
]
