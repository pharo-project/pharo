"
A PDCycleNode is used to display a cycle in a tree.
"
Class {
	#name : #DACycleNode,
	#superclass : #DANode,
	#category : #'Tool-DependencyAnalyser-UI-Nodes'
}

{ #category : #'building-menu' }
DACycleNode >> addMenuEntriesOn: aMenuGroup [
	| graphvizGenerator |

	aMenuGroup 
		addMenuItem:
			(MenuItemPresenter new
						name: 'Generate GraphViz file';
						action: [ self ifGraphVizAbsent: [ self error: 'Please install Graphviz Support' ].
									graphvizGenerator := DAPackageCycleGenerator 
										newWith: self cycle 
										named: 'CycleGraph' , self cycle size asString , 'nodes'.
									self saveGraphAndOpenWith: graphvizGenerator ])
]

{ #category : #accessing }
DACycleNode >> childNodeClassFor: anItem [
	^ DAPackageUnderAnalysisNode
]

{ #category : #accessing }
DACycleNode >> children [
	^ self content packages
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
