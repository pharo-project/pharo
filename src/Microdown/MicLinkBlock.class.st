"
I represent a reference in a text. For example `[http://pharo.org](http://pharo.org)` creates [http://pharo.org](http://pharo.org).
"
Class {
	#name : #MicLinkBlock,
	#superclass : #MicInlineBlockWithUrl,
	#category : #'Microdown-ModelInline'
}

{ #category : #visiting }
MicLinkBlock >> accept: aVisitor [
	^ aVisitor visitLink: self
]

{ #category : #visiting }
MicLinkBlock >> closeMe [
	"testAsFileUrl
	self assert: 'http://localhost:80/foo/bar/doc.txt' asZnUrl asFileUrl isFile.
	self 
		assert: 'http://localhost:80/foo/bar/doc.txt' asZnUrl path 
		equals: 'http://localhost:80/foo/bar/doc.txt' asZnUrl asFileUrl path"
	
	self url: (MicResourceReference fromString: self url).
	caption := self inlineParse: substring.
]

{ #category : #accessing }
MicLinkBlock >> kind [

 	^ #linkName
]

{ #category : #accessing }
MicLinkBlock >> openingDelimiter [

 	^ MicLinkNameOpenerDelimiter markup
]
