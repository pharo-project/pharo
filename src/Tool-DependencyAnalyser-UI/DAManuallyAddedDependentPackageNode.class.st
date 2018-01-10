"
I'm a node  representing a dependency not computed automatically but added manually on the package Manifest.
"
Class {
	#name : #DAManuallyAddedDependentPackageNode,
	#superclass : #DADependentPackageNode,
	#category : #'Tool-DependencyAnalyser-UI-Nodes'
}

{ #category : #accessing }
DAManuallyAddedDependentPackageNode >> children [
	^ self content dependencies flatCollect: 
		[ :dependency | 
		dependency users collect: 
			[ :association | | method sentMessages |
			method := association key.
			sentMessages := association value.
			DAMessageSendDependencyNode new
				content: method;
				parentNode: self;
				sentMessages: sentMessages 
				yourself ] ]
]

{ #category : #accessing }
DAManuallyAddedDependentPackageNode >> hasChildren [
	^ [ self children isNotEmpty ]
]

{ #category : #accessing }
DAManuallyAddedDependentPackageNode >> icon [
	^ self iconNamed: #smallRedoIcon
]

{ #category : #'browse-nautilus' }
DAManuallyAddedDependentPackageNode >> selectInBrowser [
	"Nothing to do"
]
