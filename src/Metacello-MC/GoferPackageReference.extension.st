Extension { #name : #GoferPackageReference }

{ #category : #'*metacello-mc' }
GoferPackageReference >> metacelloPackageNameWithBranch [
	"Answer array with package name and package name with branch name .. no branch name"

	^ { self packageName.
		 self packageName }
]
