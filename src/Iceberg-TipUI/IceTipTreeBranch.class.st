Class {
	#name : #IceTipTreeBranch,
	#superclass : #IceTipTreeItem,
	#category : #'Iceberg-TipUI-View-DataSource'
}

{ #category : #testing }
IceTipTreeBranch >> canActivateCommand: aCommandClass [
	^ aCommandClass actsOnBranch: self
]
