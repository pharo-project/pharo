"
A PDCycleNode is used to display a cycle in a tree.
"
Class {
	#name : #DACycleNode,
	#superclass : #DANode,
	#category : #'Tool-DependencyAnalyser-UI-Nodes'
}

{ #category : #'building-menu' }
DACycleNode >> addMenuEntriesOn: aMenu [
	| graphvizGenerator |

	aMenu defaultGroup
		addMenuItem: (SpMenuItemPresenter new
						name: 'Generate GraphViz file';
						action: [ treeModelParent ifGraphVizAbsent: [ self error: 'Please install Graphviz Support' ].
									graphvizGenerator := DAPackageCycleGenerator
										newWith: cycle
										named: 'CycleGraph' , cycle size asString , 'nodes'.
									treeModelParent saveGraphAndOpenWith: graphvizGenerator ])
]

{ #category : #accessing }
DACycleNode >> childNodeClassFor: anItem [
	^ DAPackageUnderAnalysisNode
]

{ #category : #accessing }
DACycleNode >> icon [
	^ self iconNamed: #refreshIcon
]

{ #category : #accessing }
DACycleNode >> isCycleNode [
	^ true
]

{ #category : #'browse-nautilus' }
DACycleNode >> selectInBrowser [
	"we cannot browse a cycle in browser"
	^ self
]
