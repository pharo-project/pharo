"
I am an abstract class to represent an open-ended syntax for special text. The syntax of the annotation is the following: 

```
	${note:value=some text|value2=""some text""}$
```

The name at the beginning of each annote is called the tag (e.g., 'index', 'note', 'cite'). The tag is followed by a series of associations (key/value pairs).

Each annotation has the possibility to define a default parameter that does not need to be explicitly tagged: 

`${mock:Foo|number=5}$` means  `${mock:value=Foo|number=5}$` since mock defines defaultParameterName as `'value'`.


### Description

I allow one to extend easily the syntax of Pillar. I am contained inside a PRDocumentGroup. I am define by a tag and I can have some parameters.

### Public API and Key Messages
- #tag This class method define the tag of an annotation.

- #parameters: aCollection   This class method is the constructor of an annotation. This method takes a dictionary of parameters.

### Internal Representation and Key Implementation Points.
 
Instance Variables
-	hadAllKeys:	I am a boolean. I  know if all the parameters written by the user had a key or if one didn't had a key. To export the document, this is useless except for the Pillar writer.
-	parameters:	I  keep all the parameters of the annotation with a key and a value.

"
Class {
	#name : #PRAbstractAnnotation,
	#superclass : #PRDocumentGroup,
	#traits : 'PRTParametrizable',
	#classTraits : 'PRTParametrizable classTrait',
	#instVars : [
		'parameters',
		'hadAllKeys',
		'isValid'
	],
	#category : #'Pillar-Core-Model'
}

{ #category : #protected }
PRAbstractAnnotation class >> acceptName: aTag [ 
	^ aTag asSymbol = self tag
]

{ #category : #constant }
PRAbstractAnnotation class >> closingAnnotationString [
	^ '}$'
				
	
]

{ #category : #accessing }
PRAbstractAnnotation class >> defaultParameterName [
	"Each annotation can have some parameters. A parameter has a key and a value. We allow for each annotation to have 1 parameter without key inside the pillar document. I return the key of this parameter."

	^ self possibleParameters first
]

{ #category : #protected }
PRAbstractAnnotation class >> findClassAcceptingTag: aTag ifNone: aBlock [
	^ self withAllConcreteClasses
		detect: [ :class | class acceptName: aTag ]
		ifNone: aBlock
]

{ #category : #accessing }
PRAbstractAnnotation class >> hasParameters [
	"Subclasses should override this method when they do not require parameters."
	^ true
]

{ #category : #testing }
PRAbstractAnnotation class >> isAbstract [
	^ self = PRAbstractAnnotation
]

{ #category : #constant }
PRAbstractAnnotation class >> keyValueSeparator [
	^ $=
				
	
]

{ #category : #constant }
PRAbstractAnnotation class >> markupParameterSeparator [
	^ $:
]

{ #category : #constant }
PRAbstractAnnotation class >> openingAnnotationString [
	^ '${'
				
	
]

{ #category : #constant }
PRAbstractAnnotation class >> parameterSeparator [
	^ $|
]

{ #category : #accessing }
PRAbstractAnnotation class >> possibleParameters [
	^ #( value )
]

{ #category : #protected }
PRAbstractAnnotation class >> tag [
	^ self possibleParameters first
]

{ #category : #protected }
PRAbstractAnnotation class >> validateParameters: parameters [
	| usedKeys |
	usedKeys := parameters keys.
	usedKeys isEmpty
		and: [ self hasParameters
				ifTrue: [ PRSyntaxError signal: 'The annotation ' , self tag , 'expect parameters from' , self possibleParameters printString , 'and none where given.' ] ].

	(usedKeys allSatisfy: [ :each | self possibleParameters includes: each asSymbol ])
		ifFalse:
			[ PRSyntaxError signal: 'The key(s) ' , (usedKeys difference: self possibleParameters) printString , '  is not valid for the annotation ' , self tag ].
	self hasParameters not and: [ usedKeys ifNotEmpty: [ PRSyntaxError signal: 'The annotation ' , self tag , ' does not expect parameter' ] ]
]

{ #category : #protected }
PRAbstractAnnotation class >> validateParameters: aPRParameters dictionary: parametersDictionary in: anAnnotation [
	| usedKeys |
	usedKeys := parametersDictionary keys.
	usedKeys isEmpty and: [ self hasParameters ifTrue: [ PRValidation strategy missingAnnotationParametersIn: anAnnotation ] ].
	(usedKeys allSatisfy: [ :each | self possibleParameters includes: each asSymbol ])
		ifFalse:
			[ PRValidation strategy invalidParameterNames: (usedKeys difference: self possibleParameters) forAnnotation: anAnnotation withParameters: aPRParameters ].
	self hasParameters not and: [ usedKeys ifNotEmpty: [ PRValidation strategy unexpectedParameterIn: anAnnotation ] ]
]

{ #category : #comparing }
PRAbstractAnnotation >> = anObject [
	^ super = anObject and: [ 
		self parameters = anObject parameters and: [ 
			self hadAllKeys = anObject hadAllKeys and: [ 
				self isValid = anObject isValid ] ] ]
]

{ #category : #visiting }
PRAbstractAnnotation >> accept: aVisitor [

	^ aVisitor visitAnnotation: self
]

{ #category : #rendering }
PRAbstractAnnotation >> asPillar [
	"Return a representation similar to the one used to create the annotation the assumption here is that the parameter dictionary only hold strings."

	^ String
		streamContents: [ :s | 
			s
				nextPutAll: self openingAnnotationString;
				nextPutAll: self tag.
			self hasParameters
				ifTrue: [ s nextPut: self markupParameterSeparator.
					self asPillarParametersOn: s ].
			s nextPutAll: self closingAnnotationString ]
]

{ #category : #rendering }
PRAbstractAnnotation >> asPillarKey: key value: aValue on: aStream [
	"Output on aStream key = aValue."

	aStream
		nextPutAll: key;
		nextPut: self keyValueSeparator;
		nextPutAll: aValue
]

{ #category : #rendering }
PRAbstractAnnotation >> asPillarParametersOn: aStream [
	| nonDefaultParameters |
	nonDefaultParameters := self parameters keys asOrderedCollection.
	"nonDefaultParameters remove: self defaultParameterName ifAbsent: [  ]."

	"first handle the case of the defaultParameter (which btw can be absent)"
	"self parameters
		at: self defaultParameterName
		ifPresent: [ :v | 
			aStream nextPutAll: v asString.
			aStream nextPut: self parameterSeparator ]."
	"Then the rest"
	nonDefaultParameters
		do: [ :each | self asPillarKey: each value: ((self parameters at: each) asString) on: aStream ]
		separatedBy: [ aStream nextPut: self parameterSeparator ]
]

{ #category : #initialization }
PRAbstractAnnotation >> beInvalid [
	isValid := false.
]

{ #category : #rendering }
PRAbstractAnnotation >> closingAnnotationString [
	^ self class closingAnnotationString
]

{ #category : #rendering }
PRAbstractAnnotation >> defaultParameterName [

	^ self class defaultParameterName
	
]

{ #category : #accessing }
PRAbstractAnnotation >> hadAllKeys [
	^ hadAllKeys ifNil: [ hadAllKeys := true ]
]

{ #category : #accessing }
PRAbstractAnnotation >> hadAllKeys: anObject [
	hadAllKeys := anObject
]

{ #category : #comparing }
PRAbstractAnnotation >> hash [
	^ ((super hash 
		bitXor: self parameters hash)
		bitXor: self hadAllKeys hash)
		bitXor: self isValid hash
]

{ #category : #initialization }
PRAbstractAnnotation >> initialize [
	super initialize.
	parameters := OrderedDictionary new.
	isValid := true.
]

{ #category : #testing }
PRAbstractAnnotation >> isValid [
	^ isValid
]

{ #category : #rendering }
PRAbstractAnnotation >> keyValueSeparator [

	^  self class keyValueSeparator
]

{ #category : #rendering }
PRAbstractAnnotation >> markupParameterSeparator [
	^ self class markupParameterSeparator
]

{ #category : #rendering }
PRAbstractAnnotation >> openingAnnotationString [
	^ self class openingAnnotationString
]

{ #category : #rendering }
PRAbstractAnnotation >> parameterSeparator [

	^  self class parameterSeparator 
]

{ #category : #accessing }
PRAbstractAnnotation >> parameters [
	^ parameters
]

{ #category : #accessing }
PRAbstractAnnotation >> parameters: aCollection [
	parameters := aCollection
]

{ #category : #accessing }
PRAbstractAnnotation >> possibleParameters [
	^ self class possibleParameters
]

{ #category : #copying }
PRAbstractAnnotation >> postCopy [
	super postCopy.
	parameters := parameters copy.
	hadAllKeys := hadAllKeys copy
]

{ #category : #rendering }
PRAbstractAnnotation >> tag [
	^ self class tag
]
