tests
testCreationManifest
	| manifestBuilder cl |
	manifestBuilder := TheManifestBuilder new.
	cl := Smalltalk globals at: #ManifestManifestResourcesTests ifAbsent: [ nil ].
	cl
		ifNotNil: [ 
			cl
				removeFromChanges;
				removeFromSystemUnlogged ].
	self assert: (manifestBuilder manifestOf: MFClassA ) isNil.
	self assert: (manifestBuilder createManifestOf: MFClassA) notNil.
	self assert: (manifestBuilder manifestOf: MFClassA) notNil