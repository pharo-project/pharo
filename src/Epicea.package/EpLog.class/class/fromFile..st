instance creation
fromFile: aFileReference 

	^ self newWithStore: (OmStoreFactory current fromFile: aFileReference)