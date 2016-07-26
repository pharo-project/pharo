actions
installStableVersion: aProject onSuccess: aBlock
	aProject ifNil: [ ^self ].
	aProject isAvailableForCurrentPharo 
		ifFalse: [  
			(UIManager default confirm: ('{1} has not been marked as tested for {2}! 
Do you want to continue (installation could fail)?' format: { aProject name. Smalltalk version }))
			ifFalse: [ ^ self ] ].
	[ 
		aProject installStableVersion.
		aBlock value ]
	on: Error do: [ :e | 
		e logCr.
		self inform: 'There was an error while trying to install ', aProject name, '. Installation was cancelled.' ]