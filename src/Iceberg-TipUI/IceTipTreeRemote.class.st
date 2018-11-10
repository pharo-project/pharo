Class {
	#name : #IceTipTreeRemote,
	#superclass : #IceTipTreeGroup,
	#category : #'Iceberg-TipUI-View-DataSource'
}

{ #category : #testing }
IceTipTreeRemote >> canActivateCommand: aCommandClass [
	^ aCommandClass actsOnRemote: self
]
