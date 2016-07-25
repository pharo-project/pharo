Variation of an MCFileTreeGitRepository, adapted for Iceberg needs. It provides the low level implementation of an IceRepository, integrating with Monticello, Git and a Cypress file tree. In the future we will have different backends for other repositories or file formats.

Internal Representation and Key Implementation Points.
    Instance Variables
	localRepository:  <FileReference> the directory where the git repository is placed
