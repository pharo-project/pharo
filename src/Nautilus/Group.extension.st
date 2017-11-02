Extension { #name : #Group }

{ #category : #'*Nautilus' }
Group >> asNautilusItemPath [
	^ Array with: self name
]

{ #category : #'*Nautilus' }
Group >> asNautilusSelection [
	^ PackageTreeGroupSelection group: self
]

{ #category : #'*Nautilus' }
Group >> restrict: anEnvironment [
	^ anEnvironment forClasses: self classes
]
