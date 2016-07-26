instance creation
of: aItem
	| mb  |

	mb := self new. 
	(mb manifestOf: aItem) 
			ifNil: [mb createManifestOf: aItem].	
	^ mb	
