I represent an interface to a git repository. 

My main responsibilities are:
- Load/update both baselines and individual packages from the repository.
- Commit changes to the local repository and publish them to a remote repository.
- Browse other versions of the loaded packages.
- Handle branches

For the Collaborators Part: State my main collaborators and one line about how I interact with them. 

Public API and Key Messages
- loadPackage: packageName
- createBranch: newBranchName

Sample usage:

    Git new origin: 'git@github.com:npasserini/pharo-git-test.git'.
    git loadPackage: 'Pharo-Git-Test'. 


Instance Variables
- origin: A string representing the url of a remote git repository (used as origin)
- repository:	An IceGitTreeGitRemoteRepository, which provides underlying git operations.
- location: <FileReference> The directory of the local repository.
- commitDictionary: <Dictionary of IceCommitInfo> Cached dictonary from commitId (hex string) to  all commits in the current branch (in the local repo).
- subdirectory: <String> The subdirectory of the local repository which is handled by the underlying GitFileTree
- versionDescriptors: <List of GitFileTreePackageEntry> cached list of all package versions saved in the (currently selected branch) of the (local) repository.
- announcer: <Announcer>
- branch: <IceBranch> currently selected branch. 
- loadedCode:  <IceLoadedCode> Contains information about the loaded code for each package in this repository. (TODO: maybe handle special cases about loading different versions loaded for different packages, see: https://github.com/npasserini/iceberg/issues/139).

Implementation Points