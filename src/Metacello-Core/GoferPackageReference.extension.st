Extension { #name : 'GoferPackageReference' }

{ #category : '*Metacello-Core' }
GoferPackageReference >> metacelloPackageNameWithBranch [
	"Answer array with package name and package name with branch name .. no branch name"

	^ { self packageName.
		 self packageName }
]
