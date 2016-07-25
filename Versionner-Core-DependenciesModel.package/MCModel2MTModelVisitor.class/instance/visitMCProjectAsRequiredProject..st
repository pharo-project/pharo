visiting
visitMCProjectAsRequiredProject: aProject
	"visit a Metacello Project to add it as a required projet. It is quite the same as visitMCProject except that we won't dive into the project structure.
	aProject is an instance of MetacelloMCProjectSpec."
	| version requiredProject |
	
	requiredProject := MTDependantProject new.
	requiredProject displayName: aProject name.
	requiredProject name: (aProject className ifNil: [aProject name] ifNotNil: [ :className | className configurationBaseName]).
	requiredProject configurationClass: aProject projectClass.
	requiredProject repositories: (aProject repositorySpecs collect: [ :aRepoSpec | aRepoSpec description]).
	requiredProject dependencies: (aProject loads ifNil: [ #() ] ifNotNil: [ :loads | loads ]) asOrderedCollection.
	
	version := MTVersion fromVersionString: aProject versionString.
	requiredProject version: version.
	
	project addDependency: requiredProject.
	^ requiredProject.