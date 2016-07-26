i/o
writeRepositoryProperties
	"When writing the repository properties, commit them as well."

	super writeRepositoryProperties.
	self gitCommand: #('add' '.filetree') in: directory.
	self gitCommand: #('commit' '-m' 'gitfiletree repository properties') in: directory