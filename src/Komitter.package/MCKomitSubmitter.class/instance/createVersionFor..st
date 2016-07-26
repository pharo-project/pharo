protocol
createVersionFor: aKomitableObject
	| versions currentAuthor |
	currentAuthor := Author fullNamePerSe.
	Author fullName: aKomitableObject author.
	[ 
	versions := aKomitableObject entities keys
		collect: [ :each | 
			| workingCopy name version patcher |
			workingCopy := each package workingCopy.
			name := workingCopy silentUniqueVersionNameIn: aKomitableObject repository.
			version := workingCopy silentlyNewVersionWithName: name message: aKomitableObject message in: aKomitableObject repository.
			workingCopy modified: each isFullyCommited not.
			patcher := MCPatcher snapshot: each patch base.
			(aKomitableObject entities at: each) do: [ :e | e operation applyTo: patcher ].
			version := MCVersion
				package: version package
				info: version info
				snapshot: patcher patchedSnapshot
				dependencies: version dependencies.
			KomitPackage removePackage: each.
			version ] ]
		ensure: [ Author fullName: currentAuthor ].
	^ versions