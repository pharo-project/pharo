checking
checkForUnknownTarget
	"check only root because other nodes with unknown target inherits it from its parent"
	self settingTreeRoots do: [:node | node item target ifNil: [node item target: (node pragma ifNotNil: [:prg | prg methodClass theNonMetaClass])]].
	self settingTreeRoots do: [:node | node checkForUnknownTarget].
