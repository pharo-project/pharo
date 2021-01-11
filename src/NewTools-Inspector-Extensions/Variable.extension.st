Extension { #name : #Variable }

{ #category : #'*NewTools-Inspector-Extensions' }
Variable >> inspectionUsingMethods [
	<inspectorPresentationOrder: 920 title: 'Using Methods'>
	| usingMethods allClassesReferencing sortBlock treeTable childrenBlock |
	
	usingMethods := self usingMethods.
	sortBlock := [ :x :y | x name < y name ]. 
	
	"Roots: classes with references to Variable"	
	allClassesReferencing := (usingMethods
		collect: [ :rbMethodDef | rbMethodDef methodClass ]) asSet asSortedCollection: sortBlock.
	
	"Children: references from selected class"
	childrenBlock := [ :aClass | 
		(usingMethods	select: [ :rbMethodDef | 
				rbMethodDef methodClass == aClass ])	asSortedCollection: sortBlock ].
	
	treeTable := SpTreeTablePresenter new.	
	treeTable
		roots: allClassesReferencing;
		children: childrenBlock.
	treeTable addColumn: (SpStringTableColumn title: 'Using Methods' evaluated: #name).
	^ treeTable
]
