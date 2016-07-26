protocol
remove
	"search if I am present in other groups and remove myself from these groups, 
	then remove myself from project dependencies"
	(self project groups select: [ :aGroup | aGroup dependencies includes: name ])
		do: [ :aGroup | aGroup removeDependency: name].
	self project dependencies: (self project dependencies reject: [ :dep | dep = self]).
	self project announcer announce: (MTProjectModelChanged project: self)