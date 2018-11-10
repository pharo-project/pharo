"
I'm an action to remove branches from remotes.
"
Class {
	#name : #IceGitHubRemoveBranchesAction,
	#superclass : #IceGitHubAction,
	#instVars : [
		'branches',
		'branchesByName'
	],
	#category : #'Iceberg-Plugin-GitHub-Commands'
}

{ #category : #execute }
IceGitHubRemoveBranchesAction >> basicExecute [ 

	credentials := IceGitHubAPI ensureCredentials.

	UIManager default 
		informUser: ('Retrieving branch information from {1}' format: { self remote url })
		during: [ self cacheAllBranches ].

	IceGitHubSelectListModel new 
		setModal: true;
		title: ('Select branches to remove on remote {1}' format: {self remote name});
		selectLabel: 'Remove branches';
		items: self remoteBranches;
		displayBlock: [ :each | 
			'{1} ({2})' format: { 
				each at: 'name'. 
				self timeSinceLastCommit: each } ];
		onAccept: [ :selection | self removeBranches: selection ];
		openWithSpec
]

{ #category : #private }
IceGitHubRemoveBranchesAction >> cacheAllBranches [
	self remoteBranches do: [ :each | | branchName |
		branchName := each at: 'name'.
		branchesByName 
			at: branchName 
			ifAbsentPut: [ self remoteBranch: branchName ] ]
]

{ #category : #initialization }
IceGitHubRemoveBranchesAction >> initialize [ 
	super initialize.
	branchesByName := OrderedDictionary new
]

{ #category : #private }
IceGitHubRemoveBranchesAction >> remoteBranch: aName [
	^ self requestRemoteBranch: aName
]

{ #category : #private }
IceGitHubRemoveBranchesAction >> remoteBranches [
	^ branches ifNil: [ branches := self requestRemoteBranches ]
]

{ #category : #actions }
IceGitHubRemoveBranchesAction >> removeBranch: json [
	| branchName |
	
	branchName := json at: 'name'.
	IceGitHubAPI new 
		credentials: self credentials;
		deleteBranch: self remote owner 
			project: self remote projectBasename
			name: branchName.
	UIManager default inform: ('Branch {1} removed.' format: { branchName })		
	
]

{ #category : #actions }
IceGitHubRemoveBranchesAction >> removeBranches: aCollection [
	[ 
		aCollection do: [ :each | self removeBranch: each ] ]
	on: IceGitHubError do: [ :e | self reportError: e ]
]

{ #category : #private }
IceGitHubRemoveBranchesAction >> requestRemoteBranch: aName [
	^ IceGitHubAPI new
		getBranch: self remote owner 
		project: self remote projectBasename
		name: aName
]

{ #category : #private }
IceGitHubRemoveBranchesAction >> requestRemoteBranches [
	^ IceGitHubAPI new
		getBranches: self remote owner 
		project: self remote projectBasename
]

{ #category : #private }
IceGitHubRemoveBranchesAction >> timeSinceLastCommit: json [
	| branchName branchJson lastCommitDate |
	branchName := json at: 'name'.
	branchJson := branchesByName at: branchName.
	lastCommitDate := ((((branchJson at: 'commit') at: 'commit') at: 'author') at: 'date') asDateAndTime.
	^ '{1} days' format: { (Date today - lastCommitDate) days }
]
