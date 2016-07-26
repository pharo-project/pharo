accessing
sortHierarchically

	^ sortHierarchically ifNil: [ sortHierarchically := self class sortHierarchically ]