Class {
	#name : #MicLocalFileResource,
	#superclass : #MicResourceReference,
	#category : #'Microdown-Core'
}

{ #category : #converting }
MicLocalFileResource >> asFileReference [

	^ self
]

{ #category : #converting }
MicLocalFileResource >> fullName [ 
	^ urlOrFileReference fullName
]

{ #category : #converting }
MicLocalFileResource >> resolvedBy: aRoot [
	
	^ (aRoot isKindOf: FileReference)
			ifTrue: [ MicLocalFileResource new 
								originalString: originalString; 
								url: (aRoot resolvePath: self path) ]
			ifFalse: [ 
					MicHTTPResource new 
								originalString: originalString;
								url: (aRoot withRelativeReference: originalString) ].
]

{ #category : #converting }
MicLocalFileResource >> resolvedBy: parentPath withResolver: aResolver [
	
	^ (aResolver resolutionSource isKindOf: ZnUrl)
		ifTrue: [ 
			"I'm http://files.pharo.org/...."
			| resolverPath |
			"needed because withRelatedReference: does not handle well http://pharo.org/figures/"
			resolverPath := Path from: aResolver resolutionSource path.
			MicHTTPResource new 
				originalString: originalString; 
				url: (aResolver resolutionSource withRelativeReference: (resolverPath resolvePath: self path ) pathString) ]
		ifFalse: [ 
			"I have a filesystem + parentPath and local path"
			| resolverPath |
			resolverPath := aResolver resolutionSource path.
			self class new 
				originalString: originalString; 
				url: (aResolver resolutionSource resolvePath: (resolverPath resolvePath: (parentPath resolvePath: self path))) ]
			
]

{ #category : #converting }
MicLocalFileResource >> resolvedWithResolver: aResolver [
	"Experimental"
	^ (aResolver resolutionSource isKindOf: ZnUrl)
		ifTrue: [ 
			"I'm http://files.pharo.org/...."
			| resolverPath |
			"needed because withRelatedReference: does not handle well http://pharo.org/figures/"
			resolverPath := Path from: aResolver resolutionSource path.
			MicHTTPResource new 
				originalString: originalString; 
				url: (aResolver resolutionSource withRelativeReference: resolverPath) ]
		ifFalse: [ 
			"I have a file reference but no filesystem (for example a pharo image) + and local path"
			| resolverPath |
			resolverPath := aResolver resolutionSource path.
			self class new 
				originalString: originalString; 
				url: (aResolver resolutionSource resolvePath: (resolverPath resolvePath: self path)) ]
			
]

{ #category : #converting }
MicLocalFileResource >> string: aString [

	super string: aString.
	urlOrFileReference := Path from: originalString
]
