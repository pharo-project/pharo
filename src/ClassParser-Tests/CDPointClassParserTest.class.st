Class {
	#name : 'CDPointClassParserTest',
	#superclass : 'CDClassDefinitionParserTest',
	#category : 'ClassParser-Tests',
	#package : 'ClassParser-Tests'
}

{ #category : 'helpers' }
CDPointClassParserTest >> classDefinitionString [

	^ 'Object << #Point
	slots: { #x. #y };
	sharedVariables: { #XX . #YY };
	package: ''Kernel-BasicObjects'''
]

{ #category : 'tests' }
CDPointClassParserTest >> className [
	self flag: #Revisit. "since it should be a string according to className"
	^ #Point
]

{ #category : 'tests' }
CDPointClassParserTest >> firstClassVariableName [

	^ 'XX'
]

{ #category : 'tests' }
CDPointClassParserTest >> firstInstanceVariableName [

	^ 'x'
]

{ #category : 'helpers' }
CDPointClassParserTest >> packageName [
	^ #'Kernel-BasicObjects'
]

{ #category : 'tests' }
CDPointClassParserTest >> secondClassVariableName [

	^ 'YY'
]

{ #category : 'tests' }
CDPointClassParserTest >> secondInstanceVariableName [

	^ 'y'
]

{ #category : 'tests' }
CDPointClassParserTest >> superclassName [
	self flag: #Revisit. "because this should be string according to superclasName method."
	^ #Object
]

{ #category : 'helpers' }
CDPointClassParserTest >> testProperties [

	self assert: classDefinition binding equals: Point binding
]
