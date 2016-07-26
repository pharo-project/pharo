examples
example1

	AthensTreeView openOn: Collection extentBlock: [ :each | (5 + each instVarNames size)@(5 + each methodDict size)  ] childsBlock: [:el | el subclasses ] 