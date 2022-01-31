"
I represent a file reference or url reference.

Here are the cases that we should handle

- external links
- figures
- inputfiles

For external links we can accept an URL that will be considered as absolute path to the resources http://server/m.mic.
For inputfile and figures we can accept

- an Url http://server/m.mic or http://server/m.png
- a relative path e.g., figures/m.png

## Implementation

url is misleading as an instance variable names since it can be either an url or a file reference.

## Warning

This is class is under heavy development. 
Some API will be dropped. Right now we just wrapped an URL to get the tests passing but we will revisit all the points.

Note that 

```
'file:///toto.png' asUrl isAbsolute 
>>> false
'file://toto.png' asUrl isAbsolute 
>>> true
```
So this is counter intuitive from a simple look.

"
Class {
	#name : #MicResourceReference,
	#superclass : #Object,
	#instVars : [
		'originalString',
		'urlOrFileReference'
	],
	#category : #'Microdown-Core'
}

{ #category : #'instance creation' }
MicResourceReference class >> fromString: aString [
	
	^ (aString beginsWith: 'http')
		ifTrue: [ MicHTTPResource new 
						string: aString; 
						yourself ]
		ifFalse: [ MicLocalFileResource new 
							string: aString;
							yourself ]
		
]

{ #category : #converting }
MicResourceReference >> asFileReference [

	^ self subclassResponsibility
]

{ #category : #testing }
MicResourceReference >> isFile [
	
	(originalString last = $/)
		ifTrue: [ ^ false ].
	^ true
]

{ #category : #testing }
MicResourceReference >> isImageFile [
	"Working both with resolved or not."
	^ #('pcx' 'bmp' 'jpeg' 'xbm' 'pnm' 'ppm' 'gif' 'pam' 'jpg' 'png' 'pbm')
		includes: urlOrFileReference asFileReference extension asLowercase
]

{ #category : #testing }
MicResourceReference >> isWeb [
	"Answer whether I'm suitable to be opened in a web browser."

	^ false
]

{ #category : #accessing }
MicResourceReference >> originalString [
	
		
	^ originalString 
]

{ #category : #accessing }
MicResourceReference >> originalString: aString [

	originalString := aString trimBoth.
	
	 
]

{ #category : #private }
MicResourceReference >> path [ 
	^ urlOrFileReference
]

{ #category : #testing }
MicResourceReference >> printOn: aStream [

	aStream nextPutAll: originalString 
]

{ #category : #accessing }
MicResourceReference >> string: aString [

	originalString := aString trimBoth
	
	 
]

{ #category : #private }
MicResourceReference >> url [ 
	^ urlOrFileReference
]

{ #category : #private }
MicResourceReference >> url: aString [
	
	urlOrFileReference := aString
]
