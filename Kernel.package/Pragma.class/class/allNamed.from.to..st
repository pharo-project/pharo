finding
allNamed: aSymbol from: aSubClass to: aSuperClass
	"Answer a collection of all pragmas found in methods of all classes between aSubClass and aSuperClass (inclusive) whose keyword is aSymbol."
	
	^ Array streamContents: [ :stream |
		aSubClass withAllSuperclassesDo: [ :class |
			self withPragmasIn: class do:  [ :pragma |
				pragma keyword = aSymbol
					ifTrue: [ stream nextPut: pragma ] ].
			aSuperClass = class
				ifTrue: [ ^ stream contents ] ] ].