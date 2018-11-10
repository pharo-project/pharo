Class {
	#name : #IceCypressPropertiesChange,
	#superclass : #Object,
	#category : #'Iceberg-Changes'
}

{ #category : #visiting }
IceCypressPropertiesChange >> accept: anIceChangeImporter [ 
	
	anIceChangeImporter visitCypressPropertiesChange: self
]
