"This scripts sets the correct versionInfo for loaded Pharo packages. The purpose of it is to ensure 
 iceberg will find an ancestor in history and it will correctly calculate differences."
| repo commit repositoryReference |

"Create repository"

repositoryReference := (Smalltalk os environment at: 'BOOTSTRAP_REPOSITORY' ifAbsent: ['..']) asFileReference.

repo := IceRepositoryCreator new
	location: repositoryReference;
	subdirectory: 'src';
	createRepository.

repo name: 'pharo'.
"get loaded commit"
commit := repo branch lastCommit.
"using commit as versionInfo for all loaded packages"
repo savedPackages do: [ :eachPackage | 
	MCWorkingCopy allManagers
		detect: [ :each | each packageName = eachPackage packageName ]
		ifFound: [ :each |
			each ancestry ancestors: { 
				IceProxyMCVersionInfo
					remoteUrl: 'git@github.com:pharo-project/pharo.git'
					packageName: each packageName
					commit: commit 
					message: ('Stub pointing to remote repository git@github.com:pharo-project/pharo.git. 
Add iceberg repository to have the real version here.')
					author: 'TheBootstrap' } ] ]