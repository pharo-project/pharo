utility
uuidFromGitSHA: cid package: aPackageName
	"Build a unique SHA1 hash out of the git commit ID and package name."

	"Should ensure:
		repeatability... same UUID everytime for a git commitID and a package name.
		unicity... no two different packages, even with the same commitID, have the same UUID."

	^ self uuidFromSHA: (SHA1 new hashStream: (ReadStream on: cid hex , aPackageName))