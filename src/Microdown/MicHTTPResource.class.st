Class {
	#name : #MicHTTPResource,
	#superclass : #MicResourceReference,
	#category : #'Microdown-Core'
}

{ #category : #converting }
MicHTTPResource >> asFileReference [

	^ urlOrFileReference asFileReference
]

{ #category : #converting }
MicHTTPResource >> fullName [

	^ urlOrFileReference printString
]

{ #category : #testing }
MicHTTPResource >> isWeb [

	^ urlOrFileReference isWeb
]

{ #category : #querying }
MicHTTPResource >> query [
	^ urlOrFileReference query
]

{ #category : #resolving }
MicHTTPResource >> resolvedBy: parentPath withResolver: aResolver [
	"I'm url resource so I do not care about a root"

	^ (aResolver resolutionSource isKindOf: ZnUrl)
			ifTrue: [
				"we want to resolve over for example http://github.com/pharo-project...."
				MicHTTPResource new 
								originalString: originalString; 
								url: (aResolver resolutionSource withRelativeReference: self path) ]
			ifFalse: [ 
				"Now I resolve again a file system... well this is http so I do not care about the resolver"
				self				
				]


]

{ #category : #resolving }
MicHTTPResource >> resolvedWithResolver: aResolver [
	"I'm url resource so I do not care about a root"
	"Experimental"
	^ (aResolver resolutionSource isKindOf: ZnUrl)
			ifTrue: [
				"we want to resolve over for example http://github.com/pharo-project...."
				MicHTTPResource new 
								originalString: originalString; 
								url: (aResolver resolutionSource withRelativeReference: self path) ]
			ifFalse: [ 
				"Now I resolve again a file system... well this is http so I do not care about the resolver"
				self				
				]


]

{ #category : #converting }
MicHTTPResource >> string: aString [

	super string: aString.
	urlOrFileReference := ZnUrlCopy fromString: originalString.
]
