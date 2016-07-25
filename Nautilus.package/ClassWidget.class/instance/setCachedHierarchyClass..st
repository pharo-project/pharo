private
setCachedHierarchyClass: aClass
    
	self cachedHierarchy: (SortHierarchically buildHierarchyForClass: aClass) 