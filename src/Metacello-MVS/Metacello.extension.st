Extension { #name : 'Metacello' }

{ #category : '*Metacello-MVS' }
Metacello >> dev: aProjectName repository: aRepositoryString [
	"Add aProjectName as a root in dev mode, loading its default groups (see dev:repository:loads:)"

	self dev: aProjectName repository: aRepositoryString loads: #(  )
]

{ #category : '*Metacello-MVS' }
Metacello >> dev: aProjectName repository: aRepositoryString loads: aCollectionOfGroups [
	"Add aProjectName as a root in dev mode: from aRepositoryString at its ref (a branch, a tag, a commit), loading aCollectionOfGroups (empty for its default ones), for this load only. Every request of the project takes it instead of the version asked; nothing installed is replaced (see MetacelloProjectSpec>>beDev). It comes after the root the baseline:, repository: and load: pieces build and the roots added before"

	self scriptExecutor addRoot: ((self scriptExecutor specNamed: aProjectName)
			 repository: aRepositoryString;
			 loads: aCollectionOfGroups asArray;
			 beDev;
			 yourself)
]

{ #category : '*Metacello-MVS' }
Metacello >> mvs [
	"Resolve with minimal version selection: semantic versions are minimums, the highest minimum of each repository wins. Versions and baselines come from the repositories Metacello creates. Metacello then records and loads the versions selected"

	self resolver: (MetacelloMVSResolver on: (MetacelloMVSGraph on: MetacelloMVSRepositorySource new))
]
