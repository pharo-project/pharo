Extension { #name : #GoferPackageReference }

{ #category : #'*metacello-mc' }
GoferPackageReference >> metacelloPackageNameWithBranch [
    "answer array with package name and package name with branch name .. no branch name"

    ^ {(self packageName).
    (self packageName)}
]
