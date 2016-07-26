protocol
commitSlice: aSlice
	| versions slice view workingCopy version packages name |

	slice := MCSliceInfo new.
	view := KomitSliceUI new.
	view openDialogWithSpec
		centered;
		modalRelativeTo: World.

	view cancelled ifTrue: [ ^ self ].
	
	slice issueNumber: view issueNumberText.
	slice title: view titleText.
	
	versions := self createVersionFor: aSlice.
	
	KomitterManager current reset.
	
	packages := versions collect: [ :each | 
		KomitterManager current 
			storeVersion: each
			for: each package.
		each package ].
	
	slice includedPackages: packages.
	workingCopy := slice makeSlice.
	
	name := workingCopy silentUniqueVersionNameIn: aSlice remote remote.
	
	version := workingCopy 
		silentlyNewSliceVersionWithName: name 
		message: aSlice message 
		in: aSlice remote remote.
		
	Cursor wait showWhile: [
		[ self
			storeVersion: version for: aSlice;
			storeDependencies: version for: aSlice
		] ensure: [ (MCVersionInspector new version: version) show ] 
	]