instance creation
fromFile: aFileReference 

	^ self newWithStore: (OmFileStore fromFile: aFileReference)