"
SUnit tests for file system paths
"
Class {
	#name : #PathTest,
	#superclass : #TestCase,
	#type : #variable,
	#category : #'FileSystem-Tests-Core'
}

{ #category : #tests }
PathTest >> testAbsolutePath [

	| path |

	self assert: (AbsolutePath new isAbsolute).
	self assert: (Path root isAbsolute).
	
	path := AbsolutePath from: 'parent/child/grandChild' delimiter: $/.
	self assert: path size equals: 3.
	self assert: (path at: 1) equals: 'parent'.
	self assert: (path at: 2) equals: 'child'.
	self assert: (path at: 3) equals: 'grandChild'.
	
	path := AbsolutePath from: '/' delimiter: $/.
	self assert: path equals: Path root.
	

]

{ #category : #tests }
PathTest >> testAbsolutePrintString [
	
	| path actual |
	path := Path / 'plonk' / 'griffle'.
	actual := path printString.
	self assert: actual equals: 'Path / ''plonk'' / ''griffle'''
]

{ #category : #tests }
PathTest >> testAbsoluteWithParents [
	| path allPaths |
	path := Path / 'plonk' / 'griffle' / 'nurb'.
	allPaths := path withParents.
	
	self assert: allPaths size equals: 4.
	self assert: allPaths first isRoot.
	self assert: allPaths second basename equals: 'plonk'.
	self assert: allPaths second size equals: 1.
	self assert: (allPaths second isChildOf: allPaths first).
	self assert: allPaths third basename equals: 'griffle'.
	self assert: allPaths third size equals: 2.
	self assert: (allPaths third isChildOf: allPaths second).
	self assert: allPaths fourth basename equals: 'nurb'.
	self assert: allPaths fourth size equals: 3.
	self assert: (allPaths fourth isChildOf: allPaths third).
	
	self assert: allPaths fourth equals: path.
	self assert: allPaths fourth == path
]

{ #category : #tests }
PathTest >> testAsReference [
	| path reference |
	path := Path * 'plonk'.
	reference := path asFileReference.
	self assert: reference class equals: FileReference.
	self assert: reference path equals: path
]

{ #category : #tests }
PathTest >> testBasename [
	| path |
	path := Path * 'plonk' / 'griffle'.
	self assert: path basename equals: 'griffle'
]

{ #category : #tests }
PathTest >> testBasenameNoParent [
	| path |

	path := Path / 'griffle'.
	self assert: path parent basename equals: '/'.

	path := Path * 'griffle'.
	self assert: path parent basename equals: '.'.

]

{ #category : #tests }
PathTest >> testBasenameWithoutExtension [
	
	| path |
	path := Path * 'plonk' / 'griffle'.
	self assert: path basenameWithoutExtension equals: 'griffle'.
	self assert: (path basenameWithoutExtension: 'griffle') equals: 'griffle'.
	self assert: (path basenameWithoutExtension: 'taz') equals: 'griffle'.
	
	path := Path * 'plonk' / 'griffle.taz'.
	self assert: path basenameWithoutExtension equals: 'griffle'.
	self assert: (path basenameWithoutExtension: 'taz') equals: 'griffle'.
	self assert: (path basenameWithoutExtension: 'griffle.taz') equals: 'griffle.taz'.
	self assert: (path basenameWithoutExtension: 'zork') equals: 'griffle.taz'.
	
	path := Path * 'plonk' / 'griffle.taz.zork'.
	self assert: path basenameWithoutExtension equals: 'griffle.taz'.
	self assert: (path basenameWithoutExtension: 'zork') equals: 'griffle.taz'.
	self assert: (path basenameWithoutExtension: 'taz.zork') equals: 'griffle'.
	self assert: (path basenameWithoutExtension: 'girffle.taz.zork') equals: 'griffle.taz.zork'.
	self assert: (path basenameWithoutExtension: 'taz') equals: 'griffle.taz.zork'.
]

{ #category : #tests }
PathTest >> testCanonicalization [

	| ref |

	ref := (Path * 'a/b/c') canonicalize.
	self assert: ref segments equals: #('a' 'b' 'c').

	ref := (Path / 'a/b/c') canonicalize.
	self assert: ref segments equals: #('a' 'b' 'c').

	ref := (Path * '../a/b/c') canonicalize.
	self assert: ref segments equals: #('..' 'a' 'b' 'c').

	ref := (Path * 'a/b/c/..') canonicalize.
	self assert: ref segments equals: #('a' 'b').

	ref := (Path / 'a/b/c/..') canonicalize.
	self assert: ref segments equals: #('a' 'b').

	ref := (Path * 'a/b/../c') canonicalize.
	self assert: ref segments equals: #('a' 'c').

	ref := (Path / 'a/b/../c') canonicalize.
	self assert: ref segments equals: #('a' 'c').

]

{ #category : #tests }
PathTest >> testCommaAddsExtension [
	| path result |
	path := Path * 'plonk' .
	result := path, 'griffle'.
	self assert: result basename equals: 'plonk.griffle'
]

{ #category : #tests }
PathTest >> testCommaAddsExtensionAgain [
	| path result |
	path := Path * 'plonk.griffle'.
	result := path, 'nurp'.
	self assert: result basename equals: 'plonk.griffle.nurp'
]

{ #category : #tests }
PathTest >> testContains [
	| ancestor descendent |
	ancestor := Path / 'plonk'.
	descendent := Path / 'plonk' / 'griffle' / 'bork'.
	self assert: (ancestor contains: descendent).
	self deny: (descendent contains: ancestor)
]

{ #category : #tests }
PathTest >> testContainsLocator [
	| ancestor descendent |
	ancestor := FileLocator imageDirectory resolve path.
	descendent := FileLocator image / 'griffle'.
	self deny: (ancestor contains: descendent).
	self deny: (descendent contains: ancestor)
]

{ #category : #tests }
PathTest >> testEqual [
	| a b |
	a := Path * 'plonk'.
	b := Path * 'plonk'.
	self deny: a == b.
	self assert: a equals: b.
]

{ #category : #tests }
PathTest >> testExtendingPath [

	| ref |

	self should: [ '/a/b' asPath / '' ] raise: Error.
	self should: [ '/a/b' asPath / nil ] raise: Error.

	ref := '/a/b/c' asPath / 'd/e'.
	self assert: ref segments equals: #('a' 'b' 'c' 'd' 'e').

	ref := '/a/b/c' asPath / 'd/e'.
	self assert: ref parent segments equals: #('a' 'b' 'c' 'd').

	ref := '/a/b/c' asPath / '../d'.
	self assert: ref segments equals:  #('a' 'b' 'c' '..' 'd').

	ref := '/a/b/c' asPath / 'd/..'.
	self assert: ref segments equals: #('a' 'b' 'c' 'd' '..').

	ref := '/a/b/c' asPath / 'd/../e'.
	self assert: ref segments equals: #('a' 'b' 'c' 'd' '..' 'e').

	ref := '/a/b/c' asPath / './d'.
	self assert: (ref segments = #('a' 'b' 'c' 'd')).

	ref := '/a/b/c' asPath / 'd/.'.
	self assert: (ref segments = #('a' 'b' 'c' 'd')).

	ref := '/a/b/c' asPath / 'd/./e'.
	self assert: ref segments equals: #('a' 'b' 'c' 'd' 'e').


]

{ #category : #tests }
PathTest >> testExtensions [
	self 
		assertCollection: (Path from: 'foo') extensions asArray
		equals: #().
	self
		assertCollection: (Path from: 'foo.tar') extensions asArray
		equals: #( 'tar' ).
	self
		assertCollection: (Path from: 'foo.tar.gz') extensions asArray
		equals: #( 'tar' 'gz').
	self
		assertCollection: (Path from: 'foo.1.tar.gz') extensions asArray
		equals: #( '1' 'tar' 'gz').
]

{ #category : #tests }
PathTest >> testFullName [

	| path |

	path := (FileSystem workingDirectory / 'book-result' / 'W01-Welcome')
				relativeToReference: FileSystem workingDirectory.
	self assert: path fullName equals: 'book-result/W01-Welcome'

]

{ #category : #tests }
PathTest >> testGrandchildOfPath [
	| griffle  nurb |
	griffle := Path / 'griffle'.
	nurb := griffle / 'plonk' / 'nurb'.
	self deny: (griffle isChildOf: nurb).
	self deny: (nurb isChildOf: griffle).
]

{ #category : #tests }
PathTest >> testIsAbsolute [
	self assert: (Path / 'plonk') isAbsolute
]

{ #category : #tests }
PathTest >> testIsAbsoluteWindowsPathReturnsFalseWhenNoWindowsAbsolutePathProvided [
  
	self deny: (Path isAbsoluteWindowsPath: 'tmp').
	self deny: (Path isAbsoluteWindowsPath: '/tmp').
	self deny: (Path isAbsoluteWindowsPath: '/tmp/test').
]

{ #category : #tests }
PathTest >> testIsAbsoluteWindowsPathReturnsTrueWhenWindowsAbsolutePathProvided [
  
	self assert: (Path isAbsoluteWindowsPath: 'A:\').
	self assert: (Path isAbsoluteWindowsPath: 'c:\').
	self assert: (Path isAbsoluteWindowsPath: 'c:\test').
]

{ #category : #tests }
PathTest >> testIsChildOfPath [
	| parent child |
	parent := Path / 'griffle'.
	child := parent / 'nurb'.
	self assert: (child isChildOf: parent).
	self deny: (parent isChildOf: child)
]

{ #category : #tests }
PathTest >> testIsChildOfReference [
	| parent child |
	parent := Path / 'griffle'.
	child := (FileSystem memory referenceTo: parent / 'nurb').
	self deny: (child isChildOf: parent).
	self deny: (parent isChildOf: child)
]

{ #category : #tests }
PathTest >> testIsEmpty [
	self assert: (Path workingDirectory) isEmpty
]

{ #category : #tests }
PathTest >> testIsNotAbsolute [
	self deny: (Path * 'plonk') isAbsolute
]

{ #category : #tests }
PathTest >> testIsNotRelative [
	self deny: (Path / 'plonk') isRelative
]

{ #category : #tests }
PathTest >> testIsNotRoot [
	self deny: (Path / 'plonk') isRoot
]

{ #category : #tests }
PathTest >> testIsRelative [
	self assert: (Path * 'plonk') isRelative
]

{ #category : #tests }
PathTest >> testIsRoot [
	self assert: Path root isRoot
]

{ #category : #tests }
PathTest >> testMakeRelative [
	
	| parent child relative |
	parent := Path / 'griffle' / 'bibb'.
	child := Path / 'griffle' / 'plonk' / 'nurp'.
	relative := parent makeRelative: child.
	self assert: relative equals: (Path parent / 'plonk' / 'nurp')
]

{ #category : #tests }
PathTest >> testMakeRelativeFrom2RelativePaths [
	"Related to issue: 14846 MakeRelative-method-applied-on-two-relative-paths-is-not-working"

	| parent child relative |
	parent := RelativePath new / 'griffle' / 'bibb'.
	child := RelativePath new / 'griffle' / 'plonk' / 'nurp'.
	relative := parent makeRelative: child.
	self assert: relative equals: Path parent / 'plonk' / 'nurp'
]

{ #category : #tests }
PathTest >> testParent [
	| path parent |
	path := (Path * 'plonk') / 'griffle'.
	parent := path parent.
	self assert: parent isRelative.
	self assert: (parent at: 1) equals: 'plonk'
]

{ #category : #tests }
PathTest >> testParentParent [
	| path  |
	path := (Path * '..') parent.
	self assert: path size equals: 2.
	self assert: (path at: 1) equals: '..'.
	self assert: (path at: 2) equals: '..'.
]

{ #category : #tests }
PathTest >> testParentResolution [
	| base relative absolute |
	base := Path / 'plonk' / 'pinto'.
	relative := Path parent / 'griffle' / 'zonk'.
	absolute := base resolve: relative.
	self assert: absolute isAbsolute.
	self assert: absolute segments equals: #('plonk' 'pinto' '..' 'griffle' 'zonk').

]

{ #category : #tests }
PathTest >> testParentUpTo [
	| a b c |
	a := Path / 'testParentUpTo' / 'A'.
	b := Path / 'testParentUpTo' / 'A' / 'B'.
	c := Path / 'testParentUpTo' / 'A' / 'B' / 'C'.
	self assert: b equals: (c parentUpTo: 'B').
	self assert: a equals: (c parentUpTo: 'A').
	self assert: Path / 'testParentUpTo' equals: (c parentUpTo: 'testParentUpTo').
	self assert: Path root equals: (c parentUpTo: 'notAParent')
]

{ #category : #tests }
PathTest >> testParse [

	| path |
	path := Path from: 'parent/child/grandChild' delimiter: $/.
	self assert: path size equals: 3.
	self assert: (path at: 1) equals: 'parent'.
	self assert: (path at: 2) equals: 'child'.
	self assert: (path at: 3) equals: 'grandChild'.
	

]

{ #category : #tests }
PathTest >> testParseBogus [
	
	| path |
	path := Path from: 'parent?<>~ \child/grandChild' delimiter: $/.
	self assert: path size equals: 2.
	self assert: (path at: 1) equals: 'parent?<>~ \child'.
	self assert: (path at: 2) equals: 'grandChild'.
	

]

{ #category : #tests }
PathTest >> testParseTrailingSlash [
	| path |
	path := Path from: 'griffle/' delimiter: $/.
	self assert: path size equals: 1.
	self assert: (path at: 1) equals: 'griffle'
]

{ #category : #tests }
PathTest >> testParseWindowsPathWithUnixDelimiters [
	
	| path |
	path := WindowsStore new pathFromString: 'C:\a/b/c'.
	self assert: path segments size equals: 4.
	self
		assertCollection: path segments
		equals: #('C:' 'a' 'b' 'c')

]

{ #category : #tests }
PathTest >> testPathString [

	| path |

	path := (FileSystem workingDirectory / 'book-result' / 'W01-Welcome')
				relativeToReference: FileSystem workingDirectory.
	self assert: path isRelative.
	self assert: path pathString equals: 'book-result/W01-Welcome'

]

{ #category : #tests }
PathTest >> testPrintPathOn [

	| pathString pathSrc path |

	"Test a Relative path"
	pathSrc := 'one/two/three'.
	path := Path from: pathSrc.
	self assert: path isRelative.
	pathString := String streamContents: [ :stream | path printPathOn: stream ].
	self assert: pathSrc equals: pathString.

	"Test an Absolute path"
	pathSrc := '/one/two/three'.
	path := Path from: pathSrc.
	self assert: path isAbsolute.
	pathString := String streamContents: [ :stream | path printPathOn: stream ].
	self assert: pathSrc equals: pathString
]

{ #category : #tests }
PathTest >> testPrintPathOnDelimiter [

	| pathString pathSrc path |

	"Test a Relative path"
	"Use an unusal delimiter to check that the default isn't hardcoded anywhere"
	pathSrc := 'one|two|three'.
	path := Path from: pathSrc delimiter: $|.
	self assert: path isRelative.
	pathString := String streamContents: [ :stream | path printPathOn: stream delimiter: $| ].
	self assert: pathSrc equals: pathString.

	"Test an Absolute path"
	"Use an unusal delimiter to check that the default isn't hardcoded anywhere"
	pathSrc := '|one|two|three'.
	path := Path from: pathSrc delimiter: $|.
	self assert: path isAbsolute.
	pathString := String streamContents: [ :stream | path printPathOn: stream delimiter: $| ].
	self assert: pathSrc equals: pathString
]

{ #category : #tests }
PathTest >> testPrintRelativeWithParent [
	| path |
	path := Path parent / 'foo'.
	self assert: (path printWithDelimiter: $/) equals: '../foo'
]

{ #category : #tests }
PathTest >> testPrintWithDelimiter [
	| path |
	path := (Path * 'plonk') / 'griffle'.
	self assert: (path printWithDelimiter: $%) equals: 'plonk%griffle'
]

{ #category : #tests }
PathTest >> testRedundantSeparators [

	| ref |

	ref := '/a/b/c' asPath / 'foo/'.
	self assert: ref segments equals: #('a' 'b' 'c' 'foo').

	ref := '/a/b/c' asPath / 'foo//'.
	self assert: ref segments equals: #('a' 'b' 'c' 'foo').

	ref := '/a/b/c' asPath / 'foo//..'.
	self assert: ref segments equals: #('a' 'b' 'c' 'foo' '..').

	ref := '/a/b/c' asPath / '..//foo'.
	self assert: ref segments equals: #('a' 'b' 'c' '..' 'foo').

	ref := '/a/b/c' asPath / 'foo//..//bar'.
	self assert: ref segments equals: #('a' 'b' 'c' 'foo' '..' 'bar')
]

{ #category : #tests }
PathTest >> testRelativeFromString [

	| path |
	
	path := Path from: 'plonk/griffle'.
	
	self assert: path isRelative.
	self assert: path size equals: 2.
	self assert: (path at: 1) equals: 'plonk'.
	self assert: (path at: 2) equals: 'griffle'.
]

{ #category : #tests }
PathTest >> testRelativeFromStringNormalization [

	| path |
	
	path := Path from: 'plonk/../griffle'.
	
	self assert: path isRelative.
	self assert: path size equals: 3.
	self assert: path segments equals: #('plonk' '..' 'griffle').
]

{ #category : #tests }
PathTest >> testRelativeFromStringNormalizationParent [

	| path |
	
	path := Path from: 'plonk/../../griffle'.
	
	self assert: path isRelative.
	self assert: path size equals: 4.
	self assert: path segments equals: #('plonk' '..' '..' 'griffle')
]

{ #category : #tests }
PathTest >> testRelativeFromStringParent [

	| path |
	
	path := Path from: '../..'.
	
	self assert: path isRelative.
	self assert: path size equals: 2.
	self assert: (path at: 1) equals: '..'.
	self assert: (path at: 2) equals: '..'.
]

{ #category : #tests }
PathTest >> testRelativePrintString [
	| path actual |
	path := Path * 'plonk' / 'griffle'.
	actual := path printString.
	self assert: actual equals: 'Path * ''plonk'' / ''griffle'''
]

{ #category : #tests }
PathTest >> testRelativeTo [
	"aPath relativeTo: aParent returns a new path relative to the parent"
	
	| parent child relative |
	parent := Path / 'griffle'.
	child := Path / 'griffle' / 'plonk' / 'nurp'.
	relative := child relativeTo: parent.
	self assert: relative equals: (Path * 'plonk' / 'nurp')
]

{ #category : #tests }
PathTest >> testRelativeToBranch [
	| parent child relative |
	parent := Path / 'griffle' / 'bibb'.
	child := Path / 'griffle' / 'plonk' / 'nurp'.
	relative := child relativeTo: parent.
	self assert: relative  equals: (Path parent / 'plonk' / 'nurp')
]

{ #category : #tests }
PathTest >> testRelativeWithParents [
	| path allPaths |
	path := Path * 'plonk' / 'griffle' / 'nurb'.
	allPaths := path withParents.
	
	self assert: allPaths size equals: 3.
	self assert: allPaths first basename equals: 'plonk'.
	self assert: allPaths first size equals: 1.
	self assert: allPaths second basename equals: 'griffle'.
	self assert: allPaths second size equals: 2.
	self assert: (allPaths second isChildOf: allPaths first).
	self assert: allPaths third basename equals: 'nurb'.
	self assert: allPaths third size equals: 3.
	self assert: (allPaths third isChildOf: allPaths second).
	self assert: allPaths third == path
]

{ #category : #tests }
PathTest >> testResolveAbsolute [
	| path |
	path := Path / 'griffle'.
	self assert: path resolve == path.
	self assert: path isAbsolute
]

{ #category : #tests }
PathTest >> testResolvePath [
		
	| path |
	path := Path / 'grandfather' / 'father' / 'child'.
	self 
		assert: (path resolvePath: Path / 'grandfather') 
		equals: (Path / 'grandfather').
	self 
		assert: (path resolvePath: Path / 'child')
		equals: (Path / 'child').
	self
		assert: (path resolvePath: Path * 'grandfather')
		equals: (Path / 'grandfather' / 'father' / 'child' / 'grandfather').
	self
		assert: (path resolvePath: Path * 'child')
		equals: (Path / 'grandfather' / 'father' / 'child' / 'child').
	self
		assert: (path resolvePath: Path * 'grandfather')
		equals: (Path / 'grandfather' / 'father' / 'child' / 'grandfather').
	self
		assert: (path resolvePath: Path * 'child')
		equals: (Path / 'grandfather' / 'father' / 'child' / 'child').
	self
		assert:  (path resolvePath: (Path parent) / '..') canonicalize
		equals: (Path / 'grandfather')
]

{ #category : #tests }
PathTest >> testResolveRelative [
	| path |
	path := Path * 'griffle'.
	self assert: path resolve == path.
	self assert: path isRelative
]

{ #category : #tests }
PathTest >> testResolveString [	

	| path result |
	path := Path * 'plonk'.
	result := path resolve: 'griffle'.
	self assert: result class equals: path class.
	self assert: result size equals: 2.
	self assert: (result at: 1) equals: 'plonk'.
	self assert: (result at: 2) equals: 'griffle'.
]

{ #category : #tests }
PathTest >> testRootParent [
	| root |
	root := Path root.
	self assert: root parent == root
]

{ #category : #tests }
PathTest >> testRootPrintString [
	| path actual |
	path := Path root.
	actual := path printString.
	self assert: actual equals: 'Path root'
]

{ #category : #tests }
PathTest >> testSiblingOfPath [
	| griffle  nurb |
	griffle := Path / 'griffle'.
	nurb := Path / 'nurb'.
	self deny: (griffle isChildOf: nurb).
	self deny: (nurb isChildOf: griffle).
]

{ #category : #tests }
PathTest >> testSimpleResolution [
	| base relative absolute |
	base := Path / 'plonk'.
	relative := (Path * 'griffle') / 'zonk'.
	absolute := base resolve: relative.
	self assert: absolute isAbsolute.
	self assert: (absolute at: 1) equals: 'plonk'.
	self assert: (absolute at: 2) equals: 'griffle'.
	self assert: (absolute at: 3) equals: 'zonk'.

]

{ #category : #tests }
PathTest >> testSlash [
	| path actual |
	path := Path * 'plonk'.
	actual := path / 'griffle'.
	self assert: actual class equals: path class.
	self assert: (actual printWithDelimiter: $/) equals: 'plonk/griffle'
]

{ #category : #tests }
PathTest >> testUnequalContent [
	| a b |
	a := Path * 'plonk'.
	b := Path * 'griffle'.
	self deny: a = b.
]

{ #category : #tests }
PathTest >> testUnequalSize [
	| a b |
	a := Path * 'plonk'.
	b := (Path * 'plonk') / 'griffle'.
	self deny: a = b.
]

{ #category : #tests }
PathTest >> testUnixAbsolutePathName [

	self assert: (Path from: '/test') isAbsolute.
	self assert: (Path from: '/etc/bin') isAbsolute.
]

{ #category : #tests }
PathTest >> testWindowsAbsolutePathName [
  
	self assert: (Path from: 'A:\') isAbsolute.
	self assert: (Path from: 'c:\') isAbsolute.
	self assert: (Path from: 'c:\test') isAbsolute.
]

{ #category : #tests }
PathTest >> testWithExtentionAddsExtension [
	| path result |
	path := Path * 'plonk'.
	result := path withExtension: 'griffle'.
	self assert: result basename equals: 'plonk.griffle'
]

{ #category : #tests }
PathTest >> testWithExtentionReplacesExtension [
	| path result |
	path := Path * 'plonk.griffle'.
	result := path withExtension: 'griffle'.
	self assert: result basename equals: 'plonk.griffle'
]

{ #category : #tests }
PathTest >> testWorkingDirPrintString [
	| path actual |
	path := Path workingDirectory. 
	actual := path printString.
	self assert: actual equals: 'Path workingDirectory'
]

{ #category : #tests }
PathTest >> testWorkingDirectoryParent [
	| path |
	path := Path workingDirectory parent.
	self assert: path size equals: 1.
	self assert: (path at: 1) equals: '..'
]
