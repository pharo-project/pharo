"This scripts sets the correct versionInfo for loaded Pharo packages. The purpose of it is to ensure 
 iceberg will find an ancestor in history and it will correctly calculate differences."
| repo commit |

"Create repository"
[
	repo := IceRepositoryCreator new
		location: FileLocator image parent;
		subdirectory: 'src';
		createRepository.
] ifError: [
	"alternative location (on CI)"	
	repo := IceRepositoryCreator new
		location: FileLocator image parent parent;
		subdirectory: 'src';
		createRepository.].

repo name: 'pharo'.
repo beSystemRepository.
repo register.

"get loaded commit"
commit := repo branch lastCommit.
"set loaded commit as reference"
repo loadedCode 
	instVarNamed: #referenceCommit 
	put: commit.
"using commit as versionInfo for all loaded packages"
repo savedPackages 
	collect: [ :eachPackage | 
		IceMCVersionInfo new 
			fromCommit: commit package: eachPackage;
			yourself ]
	thenDo: [ :eachInfo |  
			MCWorkingCopy allManagers
				detect: [ :each | each packageName = eachInfo package name ]
				ifFound: [ :each | each ancestry ancestors: { eachInfo } ] ].
"clean location (because this repo is my local repo, user will need to set their own)"
repo location: nil. 
