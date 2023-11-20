Extension { #name : 'MCWorkingCopy' }

{ #category : '*Metacello-Core' }
MCWorkingCopy >> metacelloPackageNameWithBranch [
	"answer array with package name and package name with branch name"
	ancestry ancestors isEmpty
		ifTrue: [ 
			^ {(package name).
			(package name)} ].
	^ (GoferVersionReference name: self ancestors first name) metacelloPackageNameWithBranch
]
