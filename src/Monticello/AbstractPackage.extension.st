Extension { #name : #AbstractPackage }

{ #category : #'*Monticello' }
RPackage >> asMCOrganizationDefinition [

	^ MCOrganizationDefinition packageName: self name tagNames: self tagNames
]

{ #category : #'*Monticello-RPackage' }
AbstractPackage >> isDirty [

	^ self mcWorkingCopy
		  ifNotNil: [ :copy | copy modified ]
		  ifNil: [ false ]
]

{ #category : #'*Monticello-RPackage' }
AbstractPackage >> mcPackage [
	"Normally the corresponding MCPackage should exist, but in certain cases it could happen that it does not"
	
	^ self mcWorkingCopy ifNotNil: [ :workingCopy | workingCopy package ].
	
]

{ #category : #'*Monticello-RPackage' }
AbstractPackage >> mcWorkingCopy [

	^ (MCWorkingCopy hasPackageNamed: self name)
		  ifTrue: [ MCWorkingCopy forPackageNamed: self name ]
		  ifFalse: [ nil ]
]
