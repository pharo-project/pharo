instance creation
oldFileOrNoneNamed: fileName
	"If the file exists, answer a read-only FileStream on it. If it doesn't, answer nil."

	| fullName |
	fullName := self fullName: fileName.
	^ (self concreteStream isAFileNamed: fullName)
		ifTrue: [ self concreteStream readOnlyFileNamed: fullName]
		ifFalse: [ nil].
