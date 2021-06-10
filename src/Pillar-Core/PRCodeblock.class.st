"
I represent a code block whose body is not executed. 

In Pillar syntax

```
[[[
1 + 3 			
]]]
```

But a block can also have a tag and parameters.

```
[[[language=smalltalk|caption=""addition""|label=whatever
1 + 3 			
]]]
```

#### Implementation thoughts
In pillar is in unclear that the argument values can contain formatting directives.

"
Class {
	#name : #PRCodeblock,
	#superclass : #PRText,
	#traits : 'PRTParametrizable',
	#classTraits : 'PRTParametrizable classTrait',
	#instVars : [
		'parameters',
		'caption'
	],
	#category : #'Pillar-Core-Model'
}

{ #category : #'instance creation' }
PRCodeblock class >> content: aString parameters: parameters [
	^ (self content: aString)
		parameters: parameters;
		yourself
]

{ #category : #comparing }
PRCodeblock >> = anObject [
	"We use the method language to be sure that the language is a PRAbstractScriptLanguage and not a string."
	
	^ super = anObject
		and: [ 
			self language.
			anObject language.
			self parameters = anObject parameters ]
]

{ #category : #visiting }
PRCodeblock >> accept: aVisitor [
	^ aVisitor visitCodeblock: self
]

{ #category : #'accessing - delegated' }
PRCodeblock >> caption [
	^ caption
		ifNil: [ 
			caption := self
				parameters at: 'caption'
				ifPresent: [ :string | (PRDocument parser parse: string) children first ]
				ifAbsent: [ PRParagraph with: (PRText content: '') ] ]
]

{ #category : #'accessing - delegated' }
PRCodeblock >> caption: aString [
	parameters := self parameters copyWith: 'caption' -> aString
]

{ #category : #'accessing - delegated' }
PRCodeblock >> eval: aString [
	parameters := parameters copyWith: 'eval' -> aString
]

{ #category : #accessing }
PRCodeblock >> firstLine: aString [
	parameters := parameters copyWith: 'firstLine' -> aString
]

{ #category : #accessing }
PRCodeblock >> fromFile: aString [
	parameters := parameters copyWith: 'fromFile' -> aString
]

{ #category : #testing }
PRCodeblock >> hasCaption [
	^ self parameters at: 'caption' ifPresent: [ :param | param isEmptyOrNil not ] ifAbsent: [ false ]
]

{ #category : #testing }
PRCodeblock >> hasLabel [
	| label |
	label := self parameters at: 'label' ifAbsent: [ ^ false ].
	^ label isEmptyOrNil not
]

{ #category : #testing }
PRCodeblock >> hasNoCode [
	^ (self text copyWithout: Character tab) isEmpty
]

{ #category : #testing }
PRCodeblock >> hasShortCaption [
	^ self parameters at: 'shortCaption'
		ifPresent: [ :param | param isEmptyOrNil not ]
		ifAbsent: [ false ]
]

{ #category : #comparing }
PRCodeblock >> hash [
	^ super hash bitXor: self properties hash
]

{ #category : #'accessing - delegated' }
PRCodeblock >> hideable: aString [
	parameters := parameters copyWith: 'hideable' -> aString
]

{ #category : #initialization }
PRCodeblock >> initialize [
	super initialize.
	parameters := OrderedDictionary new
]

{ #category : #testing }
PRCodeblock >> isEvaluated [
	^ self parameters at: 'eval' ifAbsent: [ false ]
]

{ #category : #testing }
PRCodeblock >> isHideable [
	^ self parameters at: 'hideable' ifPresent: [ :hideable | hideable = 'true' or: [ hideable = true ] ] ifAbsent: [ false ]
]

{ #category : #testing }
PRCodeblock >> isStructure [
	^ self parameters includesKey: 'structure'
]

{ #category : #'accessing - delegated' }
PRCodeblock >> label [
	^ self parameters at: 'label' ifAbsent: ['']
]

{ #category : #'accessing - delegated' }
PRCodeblock >> label: aString [
	parameters := parameters copyWith: 'label' -> aString
]

{ #category : #'accessing - delegated' }
PRCodeblock >> language [
	^ self
		parameters at: 'language'
		ifPresent: [ :language | 
			language isString
				ifTrue: [ 
					| realObject |
					realObject := PRAbstractScriptLanguage for: language.
					self language: realObject.
					realObject ]
				ifFalse: [ language ] ]
		ifAbsent: [ PRUnspecifiedScriptLanguage default ]
]

{ #category : #'accessing - delegated' }
PRCodeblock >> language: aLanguage [
	self parameters at: 'language' put: aLanguage
]

{ #category : #accessing }
PRCodeblock >> lastLine: aString [
	parameters := parameters copyWith: 'lastLine' -> aString
]

{ #category : #accessing }
PRCodeblock >> lineNumber: aString [
	parameters := parameters copyWith: 'lineNumber' -> aString
]

{ #category : #'accessing - parameters' }
PRCodeblock >> parameters [
	^ parameters
]

{ #category : #'accessing - parameters' }
PRCodeblock >> parameters: aCollection [

	parameters := aCollection
]

{ #category : #operations }
PRCodeblock >> removePossibleParagraph: content [
	^ content first class = PRParagraph
		ifTrue:[ content first children ] 
		ifFalse: [ content ]
]

{ #category : #'accessing - delegated' }
PRCodeblock >> shortCaption [
	^ self parameters at: 'shortCaption' ifAbsent: ['']
]

{ #category : #'accessing - delegated' }
PRCodeblock >> shortCaption: aString [ 
	parameters := parameters copyWith: 'shortCaption' -> aString
]

{ #category : #testing }
PRCodeblock >> structureName [
	^ self parameters at: 'structure'
]

{ #category : #testing }
PRCodeblock >> structureName: aName [
	^ self parameters at: 'structure' put: aName
]

{ #category : #accessing }
PRCodeblock >> text: aString [
	super text: aString withoutTrailingNewlines
]
