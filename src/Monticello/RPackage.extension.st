Extension { #name : #RPackage }

{ #category : #'*Monticello-RPackage' }
RPackage >> isDirty [
	
	| mcp |
	
	mcp := MCPackage named: self name.

	^(MCWorkingCopy registry 
		at: mcp
		ifPresent: [ :item | item modified ]
		ifAbsent: [ false ])
]

{ #category : #'*Monticello-RPackage' }
RPackage >> mcPackage [
	"Normally the corresponding MCPackage should exist, but in certain cases it could happen that it does not"
	
	^ self mcWorkingCopy ifNotNil: [ :workingCopy | workingCopy package ].
	
]

{ #category : #'*Monticello-RPackage' }
RPackage >> mcWorkingCopy [
	
	^ MCWorkingCopy allManagers 
		detect: [ :each | each package name = name ]
		ifNone: [ nil ].
	
]
