"
I am ZnHtmlOutputStream. I wrap another character write stream to offer a richer API for generating correct HTML markup.

See https://en.wikipedia.org/wiki/HTML

My streaming protocol contains the traditional write stream operations. These are raw and do not do any conversions/escaping.

My html protocols contains a rich API for generating correct HTML. 

String streamContents: [ :out | | html |
	html := ZnHtmlOutputStream on: out.
	html html5.
	html tag: #html do: [ 
		html tag: #body do: [
			html tag: #div class: #main do: [
				html tag: #p with: 'Hello World & Universe !'.
				html tag: #hr.
				html 
					tag: #em 
					attributes: #(class big id 1 disable nil) 
					with: 'The END' ] ] ] ].
	
ZnHtmlOutputStream streamContents: [ :html |
	html page: 'Hello World' do: [ 
		html tag: #div class: #main do: [
			html tag: #p with: 'Hello World & Universe !' ] ] ]

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnHtmlOutputStream,
	#superclass : #Object,
	#instVars : [
		'stream'
	],
	#category : #'Zinc-HTTP-Streaming'
}

{ #category : #'instance creation' }
ZnHtmlOutputStream class >> on: writeStream [
	^ self new
		on: writeStream;
		yourself
]

{ #category : #convenience }
ZnHtmlOutputStream class >> streamContents: block [
	"Execute block with a ZnHtmlOutputStream as argument to generate HTML, returning the resulting string"
	
	"ZnHtmlOutputStream streamContents: [ :html |
		html page: 'Hello World' do: [ 
			html tag: #p with: 'Hello World & Universe !' ] ]"
	
	^ String streamContents: [ :out |
			block value: (self on: out) ]
]

{ #category : #streaming }
ZnHtmlOutputStream >> << anObject [
	"Write anObject to the receiver, dispatching using #putOn:
	This is a shortcut for both nextPut: and nextPutAll: since anObject can be both
	the element type of the receiver as well as a collection of those elements.
	No further conversions of anObject are applied.
	Return self to accomodate chaining."

 	anObject putOn: self
]

{ #category : #'initialize-release' }
ZnHtmlOutputStream >> close [
	stream close
]

{ #category : #'private html' }
ZnHtmlOutputStream >> closeTag: tag [
	stream
		nextPut: $<;
		nextPut: $/;
		nextPutAll: tag;
		nextPut: $>
]

{ #category : #html }
ZnHtmlOutputStream >> comment: string [
	"Write an HTML/XML comment consisting of string, as in <!-- string -->"
	
	stream
		nextPutAll: '<!-- ';
		nextPutAll: string;
		nextPutAll: ' -->'
]

{ #category : #html }
ZnHtmlOutputStream >> escape: string [
	"Write string, escaping characters as needed for regular text"
	
	string do: [ :each | self escapeCharacter: each ]
]

{ #category : #html }
ZnHtmlOutputStream >> escapeAttributeValue: string [
	"Write string, escaping characters as needed for the value of an attribute"

	string do: [ :each |
		each == $"
			ifTrue: [ stream nextPutAll: '&quot;' ]
			ifFalse: [ stream nextPut: each ] ] 
]

{ #category : #html }
ZnHtmlOutputStream >> escapeCharacter: char [
	"Write char, escaping it as needed for regular text"

	char == $<
		ifTrue: [ stream nextPutAll: '&lt;' ]
		ifFalse: [ 
			char == $&
				ifTrue: [ stream nextPutAll: '&amp;' ]
				ifFalse: [ stream nextPut: char ] ]
]

{ #category : #streaming }
ZnHtmlOutputStream >> flush [
	stream flush
]

{ #category : #html }
ZnHtmlOutputStream >> format: string with: args [
	"Like String>>#format: format the string template using the args given, esacaping characters when needed for regular text"

	| input currentChar |
	input := string readStream.
	[ input atEnd ] whileFalse: [
		(currentChar := input next) == ${
			ifTrue: [ | expression index | 
				expression := input upTo: $}.
				index := Integer readFrom: expression ifFail: [ expression ].
				self escape: (args at: index) asString ]
			ifFalse: [
				currentChar == $\
					ifTrue: [ input atEnd ifFalse: [ self escapeCharacter: stream next ] ]
					ifFalse: [ self escapeCharacter: currentChar ] ] ]
]

{ #category : #html }
ZnHtmlOutputStream >> html5 [
	"Write the standard HTML5 DOCTYPE tag"
	
	stream nextPutAll: '<!DOCTYPE html>'
]

{ #category : #streaming }
ZnHtmlOutputStream >> next: count putAll: collection [	
	self 
		next: count 
		putAll: collection 
		startingAt: 1
]

{ #category : #streaming }
ZnHtmlOutputStream >> next: count putAll: collection startingAt: offset [
	"Write count characters from collection starting at offset."
	
	stream 
		next: count 
		putAll: collection 
		startingAt: offset
]

{ #category : #streaming }
ZnHtmlOutputStream >> nextPut: object [
	^ stream nextPut: object
]

{ #category : #streaming }
ZnHtmlOutputStream >> nextPutAll: collection [
	self 
		next: collection size 
		putAll: collection 
		startingAt: 1
]

{ #category : #'initialize-release' }
ZnHtmlOutputStream >> on: writeStream [
	stream := writeStream
]

{ #category : #'private html' }
ZnHtmlOutputStream >> openTag: tag [
	stream
		nextPut: $<;
		nextPutAll: tag;
		nextPut: $>
]

{ #category : #'private html' }
ZnHtmlOutputStream >> openTag: tag attribute: name value: value [
	stream
		nextPut: $<;
		nextPutAll: tag;
		space; 
		nextPutAll: name.
	value ifNotNil: [
		stream nextPutAll: '="'.
		self escapeAttributeValue: value asString.
		stream nextPut: $" ].
	stream nextPut: $>
]

{ #category : #'private html' }
ZnHtmlOutputStream >> openTag: tag attributes: attributes [
	stream
		nextPut: $<;
		nextPutAll: tag.
	self renderAttributes: attributes.
	stream nextPut: $>
]

{ #category : #html }
ZnHtmlOutputStream >> page: title do: block [
	"Write out a standard page using title, then execute block"
	
	"ZnHtmlOutputStream streamContents: [ :html | html page: 'Hello' do: [ html tag: #p with: 'World' ] ]"

	self html5; tag: #html do: [ 
		self
			tag: #head do: [ self tag: #title with: title ];
			tag: #body do: [ 
				self tag: #h1 with: title.
				block value ] ]
]

{ #category : #streaming }
ZnHtmlOutputStream >> print: object [
	object printOn: self
]

{ #category : #'private html' }
ZnHtmlOutputStream >> renderAttributes: attributes [
	attributes pairsDo: [ :name :value | 
		stream space; nextPutAll: name.
		value ifNotNil: [
			stream nextPutAll: '="'.
			self escapeAttributeValue: value asString.
			stream nextPut: $" ] ]
]

{ #category : #streaming }
ZnHtmlOutputStream >> space [
	self nextPut: Character space
]

{ #category : #html }
ZnHtmlOutputStream >> str: string [
	"Write string, escaping characters as needed for regular text"

	self escape: string
]

{ #category : #'html tags' }
ZnHtmlOutputStream >> tag: tag [
	"Write a standalone, self closing HTML tag"
	
	"<tag/>"
	
	stream
		nextPut: $<;
		nextPutAll: tag;
		nextPut: $/;
		nextPut: $>
]

{ #category : #'html tags' }
ZnHtmlOutputStream >> tag: tag attributes: attributes [
	"Write a standalone, self closing HTML tag with attributes, a collection of alternating keys and values"
	
	"<tag attr1=""value1"" ... attrN=""valueN""/>"

	stream
		nextPut: $<;
		nextPutAll: tag.
	self renderAttributes: attributes.
	stream
		nextPut: $/;
		nextPut: $>
]

{ #category : #'html tags' }
ZnHtmlOutputStream >> tag: tag attributes: attributePairs do: block [
	"Write an HTML tag with attributes, a collection of alternating keys and values, executing block to generate enclosed content" 
	
	"<tag attr1=""value1"" ... attrN=""valueN""> ... </tag>"

	self openTag: tag attributes: attributePairs.
	block value.
	self closeTag: tag
]

{ #category : #'html tags' }
ZnHtmlOutputStream >> tag: tag attributes: attributePairs with: string [
	"Write an HTML tag with attributes, a collection of alternating keys and values, using the escaped string as content" 
	
	"<tag attr1=""value1"" ... attrN=""valueN"">string</tag>"

	self openTag: tag attributes: attributePairs.
	self escape: string.
	self closeTag: tag 
]

{ #category : #'html tags' }
ZnHtmlOutputStream >> tag: tag class: cssClass do: block [
	"Write an HTML tag with class cssClass, executing block to generate enclosed content" 
	
	"<tag class=""cssClass""> ... </tag>"

	self openTag: tag attribute: #class value: cssClass.
	block value.
	self closeTag: tag
]

{ #category : #'html tags' }
ZnHtmlOutputStream >> tag: tag class: cssClass with: string [
	"Write an HTML tag with class cssClass, using the escaped string as content" 
	
	"<tag class=""cssClass"">string</tag>"

	self openTag: tag attribute: #class value: cssClass.
	self escape: string.
	self closeTag: tag 
]

{ #category : #'html tags' }
ZnHtmlOutputStream >> tag: tag do: block [
	"Write an HTML tag, executing block to generate enclosed content" 
	
	"<tag>...</tag>"

	self openTag: tag.
	block value.
	self closeTag: tag
]

{ #category : #'html tags' }
ZnHtmlOutputStream >> tag: tag id: cssId do: block [
	"Write an HTML tag with id cssId, executing block to generate enclosed content" 
	
	"<tag id=""cssId""> ... </tag>"

	self openTag: tag attribute: #id value: cssId.
	block value.
	self closeTag: tag
]

{ #category : #'html tags' }
ZnHtmlOutputStream >> tag: tag id: cssId with: string [
	"Write an HTML tag with id cssId, using the escaped string as content" 
	
	"<tag id=""cssId"">string</tag>"

	self openTag: tag attribute: #id value: cssId.
	self escape: string.
	self closeTag: tag 
]

{ #category : #'html tags' }
ZnHtmlOutputStream >> tag: tag with: string [
	"Write an HTML tag using the escaped string as content" 
	
	"<tag>string</tag>"

	self openTag: tag.
	self escape: string.
	self closeTag: tag 
]

{ #category : #accessing }
ZnHtmlOutputStream >> wrappedStream [
	^ stream
]
