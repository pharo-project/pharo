"
`MicPathResolver` resolves paths and urls of the document into absolute references based on the kind of resolutionSource and the kind of paths.

Imagine we have the following file that we want to resolve on a local disk

```
	'SUnit' / 'SUnit.mic'.
	with contents:  '![aFile](figures/setUp.png)'
```

We should get a figure whose reference is `FileSystem workingDirectory / 'SUnit' / 'figures' / 'setUp.png'`.

Here are the possible combinations

- aPath (e.g., figures/tmp.png) => full path := parent path + path
- aUrl  (`http://files.pharo.org/logos/p.png`) => full path := http://files.pharo.org/logos/p.png

then the root resolution impacts the full path:

- if this is an url e.g., `http://pharo.org/resources` we should get
  - 		`SUnit/figures/setUp.png` => `http://pharo.org/resources/SUnit/figures/setUp.png`
  -      `http://files.pharo.org/logos/p.png` => `http://files.pharo.org/logos/p.png`
- else a file system e.g., `~ducasse/pillar/books/`
  - `SUnit/figures/setUp.png` =>`~ducasse/pillar/books/SUnit/figures/setUp.png`
  - `http://files.pharo.org/logos/p.png` => `http://files.pharo.org/logos/p.png`
"
Class {
	#name : #MicPathResolver,
	#superclass : #MicrodownVisitor,
	#instVars : [
		'root',
		'originFile',
		'microdown'
	],
	#category : #'Microdown-Core'
}

{ #category : #initialization }
MicPathResolver >> initialize [ 
	
	super initialize. 
	self resolutionSource: FileSystem workingDirectory
]

{ #category : #initialization }
MicPathResolver >> microdown: aMicrodown [

	microdown := aMicrodown
]

{ #category : #initialization }
MicPathResolver >> originFile: aFile [
	"Origin file represents the file that is containing the figure or the input file we are resolving."
	
	originFile := aFile
]

{ #category : #visiting }
MicPathResolver >> resolutionSource [
	"Return an URL or a file reference."
	
	^ root 
]

{ #category : #visiting }
MicPathResolver >> resolutionSource: aRoot [
	"To set as root for resolution, an absolute URL or file reference."
	
	root := aRoot 
]

{ #category : #visiting }
MicPathResolver >> visitFigure: aFigure [
	originFile 
		ifNotNil: [ 
					aFigure resources: 
						(aFigure resources 
							resolvedBy: (Path * originFile parent pathString) 
							withResolver: self)]
		ifNil: [ aFigure resources: 
						(aFigure resources resolvedWithResolver: self)].
	^ aFigure
]

{ #category : #visiting }
MicPathResolver >> visitInputfile: anInputFile [
	
	anInputFile path: (anInputFile path resolvedBy: (Path * originFile parent pathString) withResolver: self)
]
