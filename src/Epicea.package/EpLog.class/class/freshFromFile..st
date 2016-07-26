instance creation
freshFromFile: aFileReference 

	^ self newWithStore: (OmFileStore freshFromFile: aFileReference)