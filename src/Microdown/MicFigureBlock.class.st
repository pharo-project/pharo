"
I am a **figure**.

My syntax is: `![Here is a nice caption.](https://files.pharo.org/media/logo/logo.png?micAnchor=figanchor&micWidth=80)`

You should get the following picture: 
![Here is a nice caption.](https://files.pharo.org/media/logo/logo.png?micAnchor=figanchor&micWidth=80)

I have a resources, i.e. an object referencing either a file (in relative or absolute manner) or an Url, that refers to a given image object.

### Implementation notes

What is important to see is that using ? hampers to see the difference between url arguments from microdown ones such as micAnchor and micWitdh. 
But if we use another character such as | `https://files.pharo.org/media/logo/logo.png|micanchor=figanchor&micwidth=80` then it breaks the external tools because they do ignore extra arguments in the expression with ? but they break on parsing |.
Therefore we use ? and we prefix all the microdown specific parameters with mic so that we can identify them. 
"
Class {
	#name : #MicFigureBlock,
	#superclass : #MicInlineBlockWithUrl,
	#instVars : [
		'parameters'
	],
	#category : #'Microdown-ModelInline'
}

{ #category : #converting }
MicFigureBlock >> accept: aVisitor [
 	^ aVisitor visitFigure: self
]

{ #category : #accessing }
MicFigureBlock >> arguments [
	^ parameters
]

{ #category : #accessing }
MicFigureBlock >> arguments: aDictionary [
	parameters := aDictionary
]

{ #category : #visiting }
MicFigureBlock >> closeMe [ 
	"we used this circumvoluted way to use the parsing on arguments provided by the URL parser,
	even in case of file. so the user can express tmp/figures/goo.png?width=90&anchor=foo"
	| dictionary urlString tempUrl |
	dictionary := OrderedDictionary new. 
	urlString := url asString.
	tempUrl := ZnUrlCopy fromString: urlString.
	tempUrl query ifNotNil: [ :q |
		q keysAndValuesDo: [ :k :v | dictionary at: k put: v  ] ].
	self arguments: dictionary.
	caption := self inlineParse: substring.
	"while the instance variable is named url it will host a file or http url mic resources object"
	url := MicResourceReference fromString: urlString
	
	
	
]

{ #category : #testing }
MicFigureBlock >> hasCaption [ 	
	caption ifNil: [ ^ false ].
	^ true
]

{ #category : #testing }
MicFigureBlock >> isImageFile [
	"Answer whether the file name indicates an image file."

	^ url isImageFile
]

{ #category : #testing }
MicFigureBlock >> isRelativeFilePath [
	self flag: #toRemove.
	^ self url isFile and: [ (self url asString beginsWith: 'file:///') not ]
]

{ #category : #accessing }
MicFigureBlock >> kind [
	self flag: #todo. 
	"remove me"
 	^ #figure
]

{ #category : #accessing }
MicFigureBlock >> openingDelimiter [

 	^ MicFigureNameOpenerDelimiter markup
]

{ #category : #accessing }
MicFigureBlock >> originUrl [
	^ self properties at: #originUrl ifAbsent: [ 
		^ url ]
]

{ #category : #accessing }
MicFigureBlock >> parameters [
	self deprecated: 'Use arguments instead' transformWith: '`@rec parameters' -> '`@rec arguments'.
	^ parameters 
]

{ #category : #accessing }
MicFigureBlock >> parameters: someParameters [
	self deprecated: 'Use arguments: instead' transformWith: '`@rec parameters: `@arg' -> '`@rec arguments: `@arg'.
	parameters := someParameters
]

{ #category : #printing }
MicFigureBlock >> printOn: aStream [
 	
 		aStream nextPut: $!;
 			nextPut: $[;
 			nextPutAll: self caption;
 			nextPut: $];
 			nextPut: $(.
		url printOn: aStream. 
		aStream nextPut: $).
 		
]

{ #category : #accessing }
MicFigureBlock >> query [
	^ url query
]

{ #category : #accessing }
MicFigureBlock >> resources [ 

	^ url 
]

{ #category : #accessing }
MicFigureBlock >> resources: micResources [

	url := micResources
]
