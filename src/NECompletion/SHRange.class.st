"
I associate a type with a range of characters in a String
I have these instance variables...
	start - the one based index of the first character of the range within the String.
	end - the one based index of the last character  of the range within the String.
	type - a Symbol describing the type of the range
	
A sequence of instances of me are created by an instance of SHParserST80 which can then used by an instance of  SHTextStyler to style Text. 
"
Class {
	#name : #SHRange,
	#superclass : #Object,
	#instVars : [
		'start',
		'end',
		'type'
	],
	#category : #'NECompletion-Parser'
}

{ #category : #'instance creation' }
SHRange class >> start: s end: e type: aSymbol [ 
	^ self new
		start: s
		end: e
		type: aSymbol;
		 yourself
]

{ #category : #tests }
SHRange >> asType [

	type == #symbol ifTrue: [ ^Symbol ].
	type == #character ifTrue: [ ^Character ].
	type == #string ifTrue: [ ^String ].
	type == #number ifTrue: [ ^Number ].
	type == #true ifTrue: [ ^True ].
	type == #false ifTrue: [ ^False ].
	type == #arrayEnd ifTrue: [ ^Array ].
	type == #byteArrayEnd ifTrue: [ ^ByteArray ].
	self isBlockEnd ifTrue: [ ^BlockClosure ].
	^nil
]

{ #category : #accessing }
SHRange >> end [
	^ end
]

{ #category : #accessing }
SHRange >> end: anInteger [ 
	end := anInteger
]

{ #category : #tests }
SHRange >> includesPosition: anInteger [
	^ anInteger between: start and: end
]

{ #category : #tests }
SHRange >> isArgument [

	^type == #methodArg
]

{ #category : #tests }
SHRange >> isAssignment [

	^type == #assignment or: [ type == #ansiAssignment ]
]

{ #category : #tests }
SHRange >> isBinary [
	^type == #binary
]

{ #category : #tests }
SHRange >> isBlockEnd [
	^ type beginsWith: 'blockEnd'
]

{ #category : #tests }
SHRange >> isBlockStart [
	^type beginsWith: 'blockStart'
]

{ #category : #tests }
SHRange >> isBlockTemporary [
	
	^type == #blockPatternTempVar or: [ type == #blockPatternArg ]
]

{ #category : #tests }
SHRange >> isClassVariable [

	^type == #classVar
]

{ #category : #tests }
SHRange >> isComment [
	^ type == #comment
]

{ #category : #tests }
SHRange >> isConstant [
	
	^self asType notNil
]

{ #category : #tests }
SHRange >> isGlobal [

	^type == #globalVar
]

{ #category : #tests }
SHRange >> isInstanceVariable [
	
	^type == #instVar
]

{ #category : #tests }
SHRange >> isKeyword [
	^ type == #keyword or:[type == #undefinedKeyword]
]

{ #category : #tests }
SHRange >> isOpening [
	
	^(type beginsWith: 'blockStart') or: [
		type beginsWith: 'leftParenthesis' ]
		
]

{ #category : #tests }
SHRange >> isSelf [
	
	^type == #self
]

{ #category : #tests }
SHRange >> isSeparator [
	
	^#(#methodTempBar #statementSeparator #patternUnary #patternArg #blockPatternArg #blockArgsBar #return #primitiveOrExternalCallEnd) identityIncludes: type
]

{ #category : #tests }
SHRange >> isSuper [
	
	^type == #super
]

{ #category : #tests }
SHRange >> isTemporaryVariable [
	
	^#(#tempVar #blockTempVar workspaceVar ) identityIncludes: type
]

{ #category : #tests }
SHRange >> isUnfinished [
	
	^#(#unfinishedString #unfinishedComment ) identityIncludes: type
]

{ #category : #tests }
SHRange >> isVariable [
	^ self isTemporaryVariable
		or: [self isInstanceVariable
				or: [self isSelf or:[self isSuper]]]
]

{ #category : #tests }
SHRange >> isVariablesOnly [
	^ self isAssignment
		or: [self isVariable
				or: [self isConstant
						or: [self isSeparator or:[self isBinary]]]]
]

{ #category : #accessing }
SHRange >> length [
	^ end - start + 1
]

{ #category : #printing }
SHRange >> printOn: aStream [

    super printOn: aStream. 
    aStream nextPutAll: ' ', type. 
    aStream nextPutAll: ' [', start asString, ' - ', end asString, ']'
]

{ #category : #accessing }
SHRange >> start [
	^ start
]

{ #category : #accessing }
SHRange >> start: anInteger [ 
	start := anInteger
]

{ #category : #accessing }
SHRange >> start: startInteger end: endInteger type: typeSymbol [ 
	start := startInteger.
	end := endInteger.
	type := typeSymbol
]

{ #category : #accessing }
SHRange >> stringIn: aText [
	^ (self textIn: aText) asString
]

{ #category : #accessing }
SHRange >> textIn: aText [
	^ aText copyFrom: self start to: self end
]

{ #category : #accessing }
SHRange >> type [
	^ type
]

{ #category : #accessing }
SHRange >> type: aSymbol [ 
	type := aSymbol
]
