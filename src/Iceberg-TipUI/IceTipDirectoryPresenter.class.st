"
I'm a model to choose directories.
"
Class {
	#name : #IceTipDirectoryPresenter,
	#superclass : #IceTipLocationPresenter,
	#category : #'Iceberg-TipUI-View'
}

{ #category : #actions }
IceTipDirectoryPresenter >> chooseReference [
	^ UIManager default 
		chooseDirectory: self chooseTitle 
		path: self location
]
