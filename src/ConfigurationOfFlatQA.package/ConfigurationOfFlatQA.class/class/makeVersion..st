snapshotting
makeVersion: versionName

	| versions |
	"get all current version object of the projects of interest"
	versions := self currentVersions.

	"create a 'version' configuration method"
	self createConfFor: versionName with: versions.
	"link to the latest version from symbolic #stable version"
	self createSymbolicFor: versionName.
	"copy all the current version to my repo"
	self copyVersions: versions.
	"save the latest version of myself, commit it to my repo and public repos like inboxes"
	self commitSelfWithVersion: versionName.