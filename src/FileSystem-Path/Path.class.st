"
I'm a private and abstract filesystem path, independent of the string representation used to describe paths on a specific filesystem. I provide methods for navigating the filesystem hierarchy and working with absolute and relative paths. I only refer to a concrete file or directory with regard to a specific filesystem. Normally users should not use me directly. 

API instance creation:

- #from: parses the supplied string using the default delimeter
- #from:delimiter: parses the supplied string using the supplied delimiter.
- #/ creates an absolute path from the supplied string
- #* creates a relative path from the supplied string

API path manipulation:

- #/ adds the supplied string to the receiver

"
Class {
	#name : #Path,
	#superclass : #Object,
	#type : #variable,
	#category : #'FileSystem-Path-Base'
}

{ #category : #'instance creation' }
Path class >> * aString [
	"Answer a relative path with aString as its sole segment. For example,
	Path * 'griffle' will produce the same result as ./griffle in a unix shell.
	The selector #* was chosen for it's visual similarity to $."
	
	"Note: aString is not parsed, so supplying a string like '/griffle/plonk'
	will not create an absolute path."
	
	^ RelativePath with: aString
]

{ #category : #'instance creation' }
Path class >> / aString [
	"Answer an absolute path with aString as it's sole segment. The selector
	was chosen to allow path construction with Smalltalk syntax, which 
	neverthelesss resembles paths as they appear in a unix shell. Eg.
	Path / 'griffle' / 'plonk'."
	
	aString isEmptyOrNil 
		ifTrue: [ Error signal: 'Path element cannot be empty or nil'].
	^ AbsolutePath with: aString
]

{ #category : #private }
Path class >> addElement: element to: result [
	element = '..'
		ifTrue: [ ^ self addParentElementTo: result ].
	element = ''
		ifTrue: [ ^ self addEmptyElementTo: result ].
	element = '.'
		ifFalse: [ result add: element ]
]

{ #category : #private }
Path class >> addEmptyElementTo: result [
	result isEmpty ifTrue: [result add: '']
		
]

{ #category : #private }
Path class >> addParentElementTo: result [
	(result isEmpty or: [ result last = '..' ])
		ifTrue: [ result add: '..' ]
		ifFalse: [ result removeLast ]
		
]

{ #category : #private }
Path class >> canonicalizeElements: aCollection [
	| result |
	result := OrderedCollection new.
	aCollection do: [ :element |
		self addElement: element to: result].
	^ result
]

{ #category : #encodings }
Path class >> delimeter [
	"Internal path delimeter"
	
	^$/
]

{ #category : #encodings }
Path class >> extensionDelimiter [
	"Return the extension delimiter character."
	^ $.
]

{ #category : #'instance creation' }
Path class >> from: aString [
	"Answer an instance of the receiver with the supplied path using the default delimiter"

	^ self from: aString delimiter: self delimeter
]

{ #category : #'instance creation' }
Path class >> from: aString delimiter: aDelimiterCharacter [ 
	"Answer a path composed of several elements delimited by aCharacter"
	| pathClass |
	aString isEmpty
		ifTrue: [ ^ self root ].
	
	pathClass :=  ((self isAbsolutePath: aString delimiter: aDelimiterCharacter) or: 
							[self isAbsoluteWindowsPath: aString]) 
		ifTrue: [ AbsolutePath ]
		ifFalse:[ RelativePath ].
	
	^ pathClass withAll: (aDelimiterCharacter split: aString)
]

{ #category : #private }
Path class >> isAbsolutePath: aString delimiter: aCharacter [
	"Answer a boolean indicating whether the supplied path is considered absolute"

	^aString first = aCharacter
]

{ #category : #private }
Path class >> isAbsoluteUnixPath: aString [
	^aString first = $/ 
]

{ #category : #private }
Path class >> isAbsoluteWindowsPath: aString [
	aString ifEmpty: [ ^ false ].
	(aString first = $\) ifTrue: [ ^ true ]. "e.g. \file"
	^ ((aString size > 2) and: [ aString first isLetter ])
		ifTrue: [ (aString second = $:) and: [aString third = $\] ] "e.g. C:\..."
		ifFalse: [ false ]

]

{ #category : #'instance creation' }
Path class >> parent [
	"Answer a path that resolves to the parent of the current
	working directory. This is similar to .. in unix, but doesn't
	rely on actual hardlinks being present in the filesystem."

	^ RelativePath with: '..'
]

{ #category : #'instance creation' }
Path class >> parents: anInteger [
	| path |
	path := self new: anInteger.
	1 to: anInteger do: [:i | path at: i put: '..'].
	^ path
]

{ #category : #private }
Path class >> removeRedundantSegments: segments [
	"Remove redundant elements ('.' and '') from the supplied segments"

	^segments select:
		[ :segment | segment notEmpty and: [ segment ~= '.' ] ]
]

{ #category : #'instance creation' }
Path class >> root [
	"Answer the root path - ie, / on unix"
	
	^ AbsolutePath new
]

{ #category : #private }
Path class >> with: aString [
	"Answer an instance of the receiver representing the supplied string.
	This should only be sent to one of the concrete subclasses, e.g. AbsolutePath and RelativePath"
	| inst parsedCollection |

	parsedCollection := self delimeter split: aString.
	parsedCollection := self removeRedundantSegments: parsedCollection.
	inst := self new: parsedCollection size.
	parsedCollection withIndexDo: [:segment :index |
		inst at: index put: segment].
	^ inst
]

{ #category : #private }
Path class >> withAll: aCollection [
	"Answer an instance of the receiver representing the supplied collection of strings.
	This should only be sent to one of the concrete subclasses, e.g. AbsolutePath and RelativePath"
	| inst parsedCollection |

	parsedCollection := OrderedCollection new: (aCollection size max: 10).
	aCollection do: [ :each |
		parsedCollection addAll: (self delimeter split: each) ].
	parsedCollection := self removeRedundantSegments: parsedCollection.
	inst := self new: parsedCollection size.
	parsedCollection withIndexDo: [:segment :index |
		inst at: index put: segment].
	^ inst
]

{ #category : #'instance creation' }
Path class >> workingDirectory [
	"Answer a path that will always resolve to the current working directory."
	
	^ RelativePath new
]

{ #category : #navigating }
Path >> , extension [ 
	^ self withName: self basename extension: extension
]

{ #category : #navigating }
Path >> / aString [
	"aString is either a file or path.  If aString is relative, it is appended to the receiver, if it is absolute, an instance of the receiver with the path is answered"

	aString isEmptyOrNil 
		ifTrue: [ Error signal: 'Path element cannot be empty or nil'].

	^self resolvePath: (self class from: aString)
]

{ #category : #comparing }
Path >> <= other [
	^ self fullName <= other fullName
]

{ #category : #comparing }
Path >> = other [
	^ self species = other species
		and: [self size = other size
			and: [(1 to: self size) allSatisfy: [:i | (self at: i) = (other at: i)]]]
]

{ #category : #converting }
Path >> asPath [
	^ self
]

{ #category : #converting }
Path >> asPathWith: anObject [
	^ self
]

{ #category : #navigating }
Path >> asResolvedBy: anObject [
	^ anObject resolvePath: self
]

{ #category : #accessing }
Path >> base [
	^self basenameWithoutExtension 
]

{ #category : #accessing }
Path >> basename [
	"Returns the base of the basename, 
		i.e. 
		/foo/gloops.taz basename is 'gloops.taz'
		If empty, it is the emptyPathString"

	self isEmpty ifTrue: 
		[ ^ self emptyPathString ].
	^ self at: self size
]

{ #category : #accessing }
Path >> basename: newBasename [
	"change the basename"
	self size == 0
		"the root node"
		ifTrue: [ ^ Error signal: '0 length Path, cannot change basename'].
	self at: self size put: newBasename
]

{ #category : #accessing }
Path >> basenameWithoutExtension [
	"Returns the base of the basename but without its extension, 
		i.e. 
		/foo/gloops.taz basenameWithoutExtension is 'gloops'
		/ basenameWithoutExtension is '/'"
	
	^ self basename copyUpToLast: self extensionDelimiter
]

{ #category : #accessing }
Path >> basenameWithoutExtension: anExtension [
	"Returns the basename without specified extension (if any)
	('/foo/gloops.taz' asPath basenameWithoutExtension: 'taz') = 'gloops'
	"
	| extensionWithDelimiter |

	extensionWithDelimiter := anExtension copyWithFirst: self extensionDelimiter.
	(self basename endsWith: extensionWithDelimiter)
		ifTrue: [^ self basename allButLast: extensionWithDelimiter size]
		ifFalse: [ ^ self basename ]
]

{ #category : #navigating }
Path >> canonicalize [
	"Answer the receiver with references to the current folder (.) and parent folder (..) removed"
	
	^self class withAll: (self class canonicalizeElements: self segments).
]

{ #category : #comparing }
Path >> contains: anObject [
	"Return true if anObject is in a subfolder of me"
	^ anObject isContainedBy: self
]

{ #category : #comparing }
Path >> containsPath: aPath [
	self size < aPath size ifFalse: [^ false].
	1	to: self size 
		do: [:i | (self at: i) = (aPath at: i) ifFalse: [^ false]].
	^ true
]

{ #category : #comparing }
Path >> containsReference: aReference [
	^ false
]

{ #category : #private }
Path >> copyFrom: aPath [
	| size |
	size := aPath size min: self size.
	1 to: size do: [:i | self at: i put: (aPath at: i)].
	
]

{ #category : #accessing }
Path >> delimiter [
	^ $/
]

{ #category : #enumerating }
Path >> do: aBlock [ 
	1 
		to: self size
		do: 
			[ :index || segment |
			segment := self at: index.
			segment isEmpty ifFalse: [ aBlock value: segment ] ]
]

{ #category : #accessing }
Path >> emptyPathString [
	"Answer the string representing an empty (size = 0) instance of the receiver"

	^self delimiter asString
]

{ #category : #accessing }
Path >> extension [
	"Return the extension of path basename i.e., /foo/gloops.taz extension is 'taz'"
	
	^ self basename copyAfterLast: self extensionDelimiter
]

{ #category : #accessing }
Path >> extensionDelimiter [
	^ self class extensionDelimiter
]

{ #category : #accessing }
Path >> extensions [
	"Return the extensions of the receiver in order of appearance"
	"(Path from: '/foo/bar.tar.gz') extensions"
	^ (self extensionDelimiter split: self basename) allButFirst
]

{ #category : #accessing }
Path >> fullName [
	"Return the fullName of the receiver."
	
	^ self pathString
]

{ #category : #comparing }
Path >> hash [
	| hash |
	hash := self class identityHash.
	1 to: self size do:
		[:i | hash := String stringHash: (self at: i) initialHash: hash].
	^ hash
]

{ #category : #testing }
Path >> isAbsolute [
	self subclassResponsibility 
]

{ #category : #private }
Path >> isAllParents [
	1 to: self size do: [:i | (self at: i) = '..' ifFalse: [^ false]].
	^ true
]

{ #category : #comparing }
Path >> isChildOf: anObject [
	^ self parent = anObject
]

{ #category : #comparing }
Path >> isContainedBy: anObject [
	"DoubleDispatch helper for #contains:"
	^ anObject containsPath: self
]

{ #category : #testing }
Path >> isEmpty [
	^ self size = 0
]

{ #category : #testing }
Path >> isRelative [
	^ self isAbsolute not
]

{ #category : #testing }
Path >> isRoot [
	self subclassResponsibility 
]

{ #category : #testing }
Path >> isWorkingDirectory [
	^ self size = 0
]

{ #category : #private }
Path >> lengthOfStemWith: aPath [
	| limit index |
	limit := self size min: aPath size.
	index := 1.
	[index <= limit and: [(self at: index) = (aPath at: index)]] 
		whileTrue: [index := index + 1].
	^ index - 1
]

{ #category : #navigating }
Path >> makeRelative: anObject [
	^ anObject relativeToPath: self
]

{ #category : #navigating }
Path >> parent [
	| size parent |
	self isRoot ifTrue: [^ self].
	self isAllParents ifTrue: [^ self / '..'].
	
	size := self size - 1.
	parent := self class new: size.
	1 to: size do: [:i | parent at: i put: (self at: i)].
	^ parent
]

{ #category : #navigating }
Path >> parentUpTo: aParentDirName [
	"Answers the path of the parent dir with name aParentDirName or root if not found."

	self withParents
		reverseDo: [ :dir | 
			dir basename = aParentDirName
				ifTrue: [ ^ dir ] ].
	^ Path root
]

{ #category : #printing }
Path >> pathString [
	"Return a string containing the path elements of the receiver, without the 'Path *' part"

	"((FileSystem workingDirectory / 'book-result' / 'W01-Welcome')
		relativeToReference: FileSystem workingDirectory) pathString
	>>> 'book-result/W01-Welcome'"

	^String streamContents: [ :stream | 
		self printPathOn: stream delimiter: self delimiter ]
]

{ #category : #printing }
Path >> printOn: aStream [ 
	self printOn: aStream delimiter: self delimiter.

]

{ #category : #printing }
Path >> printOn: aStream delimiter: aCharacter [
	(1 to: self size)
		do: [:index | aStream nextPutAll: (self at: index)]
		separatedBy: [aStream nextPut: aCharacter]
]

{ #category : #printing }
Path >> printPathOn: aStream [
	"Print the receiver's path on aStream (without 'Path' prepended) using the default delimiter"
	"String streamContents: [ :str| 
		((FileSystem workingDirectory / 'book-result' / 'W01-Welcome') 
			relativeToReference: FileSystem workingDirectory) printPathOn: str]
	>>> 'book-result/W01-Welcome'"

	self printPathOn: aStream delimiter: self delimiter.

]

{ #category : #printing }
Path >> printPathOn: aStream delimiter: aCharacter [
	"Print the receiver's path on aStream (without 'Path' prepended)"
	"String streamContents: [ :str| 
		((FileSystem workingDirectory / 'book-result' / 'W01-Welcome')
			relativeToReference: FileSystem workingDirectory) printPathOn: str delimiter: $|]
	>>> 'book-result|W01-Welcome'"

	(1 to: self size)
		do: [:index | aStream nextPutAll: (self at: index)]
		separatedBy: [aStream nextPut: aCharacter]
]

{ #category : #printing }
Path >> printWithDelimiter: aCharacter [
	^ String streamContents: [:out | self printOn: out delimiter: aCharacter]
]

{ #category : #navigating }
Path >> relativeTo: anObject [
	^ anObject makeRelative: self
]

{ #category : #navigating }
Path >> relativeToPath: aPath [
	"Return the receiver as relative to the argument aPath"

	"(Path / 'griffle' / 'plonk' / 'nurp') 
		relativeToPath: (Path / 'griffle') 
			returns  plonk/nurp"

	| prefix relative |
	prefix := self lengthOfStemWith: aPath.
	relative := RelativePath parents: aPath size - prefix.
	prefix + 1 to: self size do: [ :i | relative := relative / (self at: i) ].
	^ relative
]

{ #category : #navigating }
Path >> resolve [
	^ self
]

{ #category : #navigating }
Path >> resolve: anObject [
	"Return a path in which the argument has been interpreted in the context of the receiver. Different 
	argument types have different resolution semantics, so we use double dispatch to resolve them correctly."
	
	^ anObject asResolvedBy: self
]

{ #category : #navigating }
Path >> resolvePath: aPath [
	"Answers a path created by resolving the argument against the receiver.
	If the argument is abolute answer the argument itself. Otherwise, concatenate the
	two paths."

	| elements |

	aPath isAbsolute ifTrue: [^ aPath].
	elements := Array new: self size + aPath size.

	1 to: self size do: [:i | elements at: i put: (self at: i)].
	1 to: aPath size do: [:i | elements at: self size + i put: (aPath at: i)].
	
	^ self class withAll: elements
]

{ #category : #navigating }
Path >> resolveReference: aReference [
	^ aReference
]

{ #category : #navigating }
Path >> resolveString: aString [
	"Treat strings as relative paths with a single element."
	
	^ self / aString
]

{ #category : #accessing }
Path >> segments [
	"return an array with all the path segements separated"
	| segments |
	
	segments := OrderedCollection new.
	
	self do: [ :part|
		segments add: part
	].
	
	^ segments asArray 
]

{ #category : #navigating }
Path >> withExtension: extension [ 
	| basename name |
	basename := self basename.
	^ (basename endsWith: extension) 
		ifTrue: [ self ]
		ifFalse: 
			[name := basename copyUpToLast: self extensionDelimiter.
			self withName: name extension: extension]
]

{ #category : #private }
Path >> withName: name extension: extension [
	| basename |
	basename :=String streamContents:
		[:out |
		out nextPutAll: name.
		out nextPut: self extensionDelimiter.
		out nextPutAll: extension].
	^ self copy 
		at: self size put: basename;
		yourself
]

{ #category : #enumerating }
Path >> withParents [
	| paths |
	paths := OrderedCollection new.
	1 to: self size -1 do: [ :index | paths add: ((self class new: index) copyFrom: self) ].
	paths add: self.
	
	^ paths
]
