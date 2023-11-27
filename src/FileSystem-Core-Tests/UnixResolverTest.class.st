"
SUnit tests for PlatformResolver
"
Class {
	#name : 'UnixResolverTest',
	#superclass : 'FileSystemResolverTest',
	#category : 'FileSystem-Core-Tests-Resolver',
	#package : 'FileSystem-Core-Tests',
	#tag : 'Resolver'
}

{ #category : 'running' }
UnixResolverTest >> createResolver [

	^ UnixResolver new
]

{ #category : 'running' }
UnixResolverTest >> testGetTempFromTMPDIR [

	| expected |
	OSPlatform current isUnix ifFalse: [ ^ self skip ].
	
	expected := '/tmp/foo'.
	OSEnvironment current setEnv: 'TMPDIR' value: expected during: [ 
		| actual |
		actual := self createResolver temp.
		self assert: actual fullName equals: expected ]

]

{ #category : 'tests' }
UnixResolverTest >> testXdgParseUserDirLineDocuments [
	"Ensure that a path of the form '$HOME/Documents' answers the expected value.
	Note that this test can be run on any platform."
	| stream path relativePath |

	stream := '"$HOME/Documents"' readStream.
	path := resolver xdgParseUserDirLine: stream.
	relativePath := path relativeTo: resolver home.
	self assert: relativePath pathString equals: 'Documents'
]

{ #category : 'tests' }
UnixResolverTest >> testXdgParseUserDirLineNotRoot [
	"Ensure that a path of the form '$HOME/' doesn't resolve to the root directory.
	Note that this test can be run on any platform."
	| stream path |

	stream := '"$HOME/"' readStream.
	path := resolver xdgParseUserDirLine: stream.
	self deny: path isRoot
]
