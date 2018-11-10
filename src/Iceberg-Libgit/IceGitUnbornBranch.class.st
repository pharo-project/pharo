Class {
	#name : #IceGitUnbornBranch,
	#superclass : #IceUnbornBranch,
	#category : #'Iceberg-Libgit-Core'
}

{ #category : #accessing }
IceGitUnbornBranch >> branchNamePrefix [

	^ 'refs/heads/'
]

{ #category : #accessing }
IceGitUnbornBranch >> gitRef [
	"When the repository is unborn, the repository has a single ref that points to no commits.
	The HEAD file then contains some content such as:
	
		ref: refs/heads/[branch_name]
	"

	^ ((repository location / '.git' / 'HEAD') contents withoutPrefix: 'ref: ') trimBoth
]

{ #category : #accessing }
IceGitUnbornBranch >> name [
	"Remove the refs/heads prefix from the full branch name"
	^ self gitRef withoutPrefix: self branchNamePrefix
]
