"
I represent an annotation delimted by `<?` and '?>`: a tag with parameters.

The following is a citation annotation with as parameters the name variable bound to the string 'Duca99a'.

```
?{citation|name=Duca99a}?
```

```
MicroDownParser parse: '?{citation|name=Duca99a}?'

```
"
Class {
	#name : #MicAnnotationBlock,
	#superclass : #MicInlineElement,
	#instVars : [
		'name',
		'arguments'
	],
	#category : #'Microdown-ModelInline'
}

{ #category : #'instance creation' }
MicAnnotationBlock class >> from: aStartInteger to: anEndInteger withSubstring: aString withChildren: aChildren [
	
	| splitter tag klass |
	splitter := MicArgumentsSplitter withString: aString.
	tag := splitter name trimBoth.
	self flag: #fixme.
	"we should pass the splitter and not reparse it in closeMe"
	
	klass := self subclasses detect: [ :each | each tag = tag ] ifNone: [ self ].

	^ klass new 
		start: aStartInteger; 
		end: anEndInteger; 
		substring: aString; 
		children: aChildren; 
		cleanSubstring; 
		closeMe; 
		yourself.	
]

{ #category : #accessing }
MicAnnotationBlock class >> tag [
	^ #empty
]

{ #category : #visiting }
MicAnnotationBlock >> accept: aVisitor [
	^ aVisitor visitAnnotation: self
]

{ #category : #accessing }
MicAnnotationBlock >> arguments [
	^ arguments
]

{ #category : #accessing }
MicAnnotationBlock >> arguments: anArguments [
	arguments := anArguments
]

{ #category : #visiting }
MicAnnotationBlock >> closeMe [ 
	| splitter |
	splitter := MicArgumentsSplitter withString: substring.
	name := splitter name trimBoth.
	arguments := splitter arguments.
]

{ #category : #accessing }
MicAnnotationBlock >> closingDelimiter [

 	^ MicAnnotationCloserDelimiter markup
]

{ #category : #initialization }
MicAnnotationBlock >> initialize [ 
	super initialize.
	arguments := OrderedDictionary new
]

{ #category : #accessing }
MicAnnotationBlock >> kind [

	^ #annotation
]

{ #category : #accessing }
MicAnnotationBlock >> name [
	^ name
]

{ #category : #accessing }
MicAnnotationBlock >> name: aName [
	name := aName
]

{ #category : #accessing }
MicAnnotationBlock >> openingDelimiter [

 	^ MicAnnotationOpenerDelimiter markup
]

{ #category : #accessing }
MicAnnotationBlock >> parameterAt: aKey put: aValue [
	^ arguments at: aKey put: aValue
]

{ #category : #accessing }
MicAnnotationBlock >> printOn: aStream [

	aStream nextPutAll: self openingDelimiter.
	aStream nextPutAll: self substring.
	aStream nextPutAll: self closingDelimiter.
]
