announcement handling
classRemoved: aClassRemovedAnnouncement

	((aClassRemovedAnnouncement classRemoved inheritsFrom: RBLintRule) and: [ 	
			rules anySatisfy: [ :rule | 
				rule class == aClassRemovedAnnouncement classAffected 
			]]
	)ifTrue: [ self resetRules ]