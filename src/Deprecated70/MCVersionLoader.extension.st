Extension { #name : #MCVersionLoader }

{ #category : #'*Deprecated70' }
MCVersionLoader >> warnAboutLosingChangesTo: versionCollection [
	self deprecated: 'This method is dead code in Pharo and will be removed. You can use #warnAboutLosingChangesTo:ifCancel:ifMerge: instead'.
	^ (MCMergeOrLoadWarning signalFor: versionCollection).
]
