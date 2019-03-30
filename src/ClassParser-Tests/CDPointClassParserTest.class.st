Class {
	#name : #CDPointClassParserTest,
	#superclass : #CDClassDefinitionParserTest,
	#category : #'ClassParser-Tests'
}

{ #category : #helpers }
CDPointClassParserTest >> classDefinitionString [

	^ 'Object subclass: #Point
	instanceVariableNames: ''x y''
	classVariableNames: ''XX YY''
	package: ''Kernel-BasicObjects'''
]

{ #category : #tests }
CDPointClassParserTest >> className [
	self flag: #Revisit. "since it should be a string according to className" 
	^ #Point
]

{ #category : #tests }
CDPointClassParserTest >> firstClassVariableName [

	^ 'XX'
]

{ #category : #tests }
CDPointClassParserTest >> firstInstanceVariableName [

	^ 'x'
]

{ #category : #helpers }
CDPointClassParserTest >> packageName [ 
	^ #'Kernel-BasicObjects'
]

{ #category : #tests }
CDPointClassParserTest >> secondClassVariableName [

	^ 'YY'
]

{ #category : #tests }
CDPointClassParserTest >> secondInstanceVariableName [

	^ 'y'
]

{ #category : #tests }
CDPointClassParserTest >> superclassName [
	self flag: #Revisit. "because this should be string according to superclasName method."
	^ #Object 
]

{ #category : #tests }
CDPointClassParserTest >> testClassDefFromLegacyStringHasSlots [
	
	self assert: classDefinition slotNodes first name equals: self firstInstanceVariableName.
	self assert: classDefinition slotNodes second name equals: self secondInstanceVariableName.
]

{ #category : #helpers }
CDPointClassParserTest >> testProperties [

	self assert: classDefinition binding equals: Point binding
]
