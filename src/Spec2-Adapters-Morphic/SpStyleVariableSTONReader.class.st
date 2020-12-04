Class {
	#name : #SpStyleVariableSTONReader,
	#superclass : #SpStyleSTONReader,
	#category : #'Spec2-Adapters-Morphic-StyleSheet'
}

{ #category : #parsing }
SpStyleVariableSTONReader >> isContainedValueChar: char [

	^ char = $(
]

{ #category : #parsing }
SpStyleVariableSTONReader >> parseContainedValue [
	| result | 

	self parseContainedValueDo: [ :aValue | result := aValue ].

	^ result

]

{ #category : #parsing }
SpStyleVariableSTONReader >> parseContainedValueDo: block [

	self expectChar: $(.
	(self matchChar: $)) ifTrue: [ ^ self ]. "short cut for empty value"
	[ readStream atEnd ] whileFalse: [
		block cull: self parseValue.
		(self matchChar: $)) ifTrue: [ ^ self ]  ].

	self error: 'end of contained value expected'
]

{ #category : #parsing }
SpStyleVariableSTONReader >> parseNamedInstVarsFor: anObject [

	self parseMapDo: [ :instVarName :value |
		anObject 
			instVarNamed: instVarName asString 
			put: value asStyleVariable ]
]

{ #category : #parsing }
SpStyleVariableSTONReader >> parseSimpleValue [
	| char |

	readStream atEnd ifFalse: [ 
		char := readStream peek.
		(self isContainedValueChar: char) ifTrue: [ 
			^ self parseContainedValue ] ].

	^ super parseSimpleValue
]

{ #category : #private }
SpStyleVariableSTONReader >> styleClasses [

	^ super styleClasses, SpStyleAbstractVariable allSubclasses
]
