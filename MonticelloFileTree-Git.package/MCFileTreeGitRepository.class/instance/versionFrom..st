accessing
versionFrom: aVersionReferenceAsString
	"gitfiletree:// provides all version names as found by git."

	"Should be part of gofer, but isn't."

	"Since gofer is text based, we can't give him git entries, but strings and those are then rematched to get the right entry."

	| mcR packageName entry |
	packageName := (self class parseName: aVersionReferenceAsString) first , self packageExtension.
	mcR := MCFileTreeGitStReader on: directory fileName: packageName.
	mcR commitID: (entry := self allFileNames detect: [ :e | e name = aVersionReferenceAsString ]) commitID.
	mcR info: entry info.
	mcR repository: self.
	^ mcR version