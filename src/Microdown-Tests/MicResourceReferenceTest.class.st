Class {
	#name : #MicResourceReferenceTest,
	#superclass : #TestCase,
	#category : #'Microdown-Tests-Resources'
}

{ #category : #tests }
MicResourceReferenceTest >> resolver: aFileSystemOrURL [

	^ MicPathResolver new resolutionSource: aFileSystemOrURL; yourself
]

{ #category : #helper }
MicResourceReferenceTest >> resourceClass [

	^ MicResourceReference
]

{ #category : #tests }
MicResourceReferenceTest >> testIsFileOfNoUrlFile [

	| mf |
	mf := self resourceClass fromString: 'server.org/figures/p.png'.
	self assert: mf isFile.
	mf := self resourceClass fromString: '/server.org/figures/p.png'.
	self assert: mf isFile.
]

{ #category : #tests }
MicResourceReferenceTest >> testIsFileOfNoUrlFolder [

	| mf |
	mf := self resourceClass fromString: 'server.org/figures/'.
	self deny: mf isFile.
	mf := self resourceClass fromString: '/server.org/figures/'.
	self deny: mf isFile.
]

{ #category : #tests }
MicResourceReferenceTest >> testIsFileOfResolvedByNoUrlFile [

	| mf |
	mf := self resourceClass fromString: 'server.org/figures/p.png'.
	mf resolvedBy: (Path * '') withResolver: (self resolver: FileSystem workingDirectory).
	self assert: mf isFile.
	
	mf := self resourceClass fromString: '/server.org/figures/p.png'.
	mf resolvedBy: (Path * '') withResolver: (self resolver: FileSystem workingDirectory).
	self assert: mf isFile.
]

{ #category : #tests }
MicResourceReferenceTest >> testIsFileOfResolvedByNoUrlFolder [

	| mf |
	mf := self resourceClass fromString: 'server.org/figures/'.
	mf resolvedBy: (Path * '') withResolver: (self resolver: FileSystem workingDirectory).
	self deny: mf isFile.
	mf := self resourceClass fromString: '/server.org/figures/'.
	mf resolvedBy: (Path * '') withResolver: (self resolver: FileSystem workingDirectory).
	self deny: mf isFile.
]

{ #category : #tests }
MicResourceReferenceTest >> testIsFileOfResolvedUrlFile [

	| mf |
	mf := self resourceClass fromString: 'http://server/figures/m.png'.
	mf resolvedBy: (Path * '') withResolver: (self resolver: FileSystem workingDirectory).
	self assert: mf isFile.
	
]

{ #category : #tests }
MicResourceReferenceTest >> testIsFileOfUrlFile [

	| mf |
	mf := self resourceClass fromString: 'http://server/figures/m.png'.
	self assert: mf isFile.
	
]

{ #category : #tests }
MicResourceReferenceTest >> testIsFileOfUrlFolder [

	| mf |
	mf := self resourceClass fromString: 'http://server.org/figures/'.
	self deny: mf isFile.
	
]

{ #category : #tests }
MicResourceReferenceTest >> testIsFileOfUrlResolvedFolder [

	| mf |
	mf := self resourceClass fromString: 'http://server.org/figures/'.
	mf resolvedBy: (Path * '') withResolver: (self resolver: FileSystem workingDirectory).
	self deny: mf isFile.
	
]

{ #category : #tests }
MicResourceReferenceTest >> testParseFullUrl [

	| mf |
	mf := self resourceClass fromString: 'http://server/m.mic'.
	self assert: mf asString equals: 'http://server/m.mic'.

]

{ #category : #'tests - file reference conversion' }
MicResourceReferenceTest >> testResolvedFileReferenceFromFile [
	"Resolution does not change the file reference conversion."
	
	| mf |
	mf := self resourceClass fromString: '/tmp'.
	mf resolvedBy: (Path * '') withResolver: (self resolver: FileSystem workingDirectory).
	self assert: mf asFileReference printString equals: '/tmp'.
	
]

{ #category : #tests }
MicResourceReferenceTest >> testResolvedIsAbsoluteFile [
	
	| mf |
	mf := self resourceClass fromString: '/chapter1/figures/m.mic'.
	mf resolvedBy: (Path * '') withResolver: (self resolver:  FileSystem memory / 'root2').
	self assert: (mf isKindOf: MicLocalFileResource).
	self assert: mf path pathString equals:  '/chapter1/figures/m.mic'. 
	
]

{ #category : #tests }
MicResourceReferenceTest >> testResolvedIsImageFile [
	
	| mf |
	mf := self resourceClass fromString: '/chapter1/figures/m.png'.
	mf resolvedBy: (Path * '') withResolver: (self resolver:  FileSystem workingDirectory).
	self assert: mf isImageFile.
	
]

{ #category : #tests }
MicResourceReferenceTest >> testResolvedIsRelativeFile [
	
	| mf |
	mf := self resourceClass fromString: 'chapter1/figures/m.mic'.
	mf resolvedBy: (Path * '') withResolver: (self resolver:  FileSystem memory workingDirectory).

	self assert: (mf isKindOf: MicLocalFileResource).
	self assert: mf path pathString equals: 'chapter1/figures/m.mic'
	
]

{ #category : #tests }
MicResourceReferenceTest >> testUnResolvedIsRelativeFile [
	
	| mf |
	mf := self resourceClass fromString: 'chapter1/figures/m.mic'.
	self assert: (mf isKindOf: MicLocalFileResource).
	self assert: mf path pathString equals: 'chapter1/figures/m.mic'.
	
]

{ #category : #'tests - file reference conversion' }
MicResourceReferenceTest >> testUnresolvedFileReferenceFromFile [
	"Resolution does not change the file reference conversion."
	
	| mf |
	mf := self resourceClass fromString: '/chapter1/figures/m.mic'.
	self assert: mf path pathString equals: '/chapter1/figures/m.mic'.
	
]

{ #category : #tests }
MicResourceReferenceTest >> testUnresolvedIsAbsoluteFile [
	
	| mf |
	mf := self resourceClass fromString: '/chapter1/figures/m.mic'.
	self assert: (mf isKindOf: MicLocalFileResource).
	self assert: mf path pathString equals: '/chapter1/figures/m.mic'.

	
]

{ #category : #tests }
MicResourceReferenceTest >> testUnresolvedIsImageFile [
	
	| mf |
	mf := self resourceClass fromString: '/chapter1/figures/m.png'.
	self assert: mf isImageFile.
	
]

{ #category : #tests }
MicResourceReferenceTest >> testUrlGetResolved [

	
	| mf |
	mf := self resourceClass fromString: 'http://files.pharo.org/logo/pharologo.png'.
	mf resolvedBy: (Path * '') withResolver: (self resolver:  FileSystem memory workingDirectory).
	self assert: (mf isKindOf: MicHTTPResource).
	self assert: (mf url printString) equals: 'http://files.pharo.org/logo/pharologo.png'
	
]
