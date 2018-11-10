Class {
	#name : #IceTipTreeTag,
	#superclass : #IceTipTreeItem,
	#category : #'Iceberg-TipUI-Model'
}

{ #category : #testing }
IceTipTreeTag >> canActivateCommand: aCommandClass [
	^ aCommandClass actsOnTag: self
]
