external projects
fileSystemLegacy: spec
	"This is an example of an external project which does not have a configuration yet.
	Note that the package name is only used in the Metacello configuration and does not have be exactly the same as the Monticello project/version name."
	spec package: 'FileSystemLegacy' with: [
		spec 
			repository: 'http://smalltalkhub.com/mc/PharoExtras/FileSystemLegacy/main';
			"if you do not specify a version, automatically the newest version is chose."
			file:  'FileSystem-Legacy-JohanBrichau.2' ]