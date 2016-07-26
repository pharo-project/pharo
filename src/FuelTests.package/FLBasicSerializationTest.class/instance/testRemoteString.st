tests-strings
testRemoteString
	"Tests that the materialized RemoteString has *the same* location on file."

	| aRemoteString materializedRemoteString |	
	aRemoteString := SourceFiles remoteStringAt: thisContext method sourcePointer.

	materializedRemoteString := self resultOfSerializeAndMaterialize: aRemoteString.

	self assert: aRemoteString string = materializedRemoteString string.
	self assert: aRemoteString sourcePointer == materializedRemoteString sourcePointer.