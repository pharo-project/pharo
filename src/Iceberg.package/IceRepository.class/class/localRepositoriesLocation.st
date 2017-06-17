accessing
localRepositoriesLocation
	^ ((FileLocator respondsTo: #localDirectory) 
		ifTrue: [ FileLocator localDirectory / #iceberg ]
		ifFalse: [ FileLocator imageDirectory / #'iceberg-cache' ]) asFileReference