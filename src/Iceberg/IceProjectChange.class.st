"
I represent the fact that the project changed. I'm using during the double-dispatch of diff calculation, so concrete projects can add themselves to the change calculation.
"
Class {
	#name : #IceProjectChange,
	#superclass : #Object,
	#category : #'Iceberg-Project'
}

{ #category : #visiting }
IceProjectChange >> accept: anIceChangeImporter [ 
	
	anIceChangeImporter visitProjectChange: self
]
