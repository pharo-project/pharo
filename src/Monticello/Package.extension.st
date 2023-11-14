Extension { #name : 'Package' }

{ #category : '*Monticello' }
Package >> asMCOrganizationDefinition [

	^ MCOrganizationDefinition packageName: self name tagNames: self tagNames
]

{ #category : '*Monticello-Package' }
Package >> isDirty [
	
	| mcp |
	
	mcp := MCPackage named: self name.

	^(MCWorkingCopy registry 
		at: mcp
		ifPresent: [ :item | item modified ]
		ifAbsent: [ false ])
]

{ #category : '*Monticello-Package' }
Package >> mcPackage [
	"Normally the corresponding MCPackage should exist, but in certain cases it could happen that it does not"
	
	^ self mcWorkingCopy ifNotNil: [ :workingCopy | workingCopy package ].
	
]

{ #category : '*Monticello-Package' }
Package >> mcWorkingCopy [

	^ (MCWorkingCopy hasPackageNamed: self name)
		  ifTrue: [ MCWorkingCopy forPackageNamed: self name ]
		  ifFalse: [ nil ]
]
