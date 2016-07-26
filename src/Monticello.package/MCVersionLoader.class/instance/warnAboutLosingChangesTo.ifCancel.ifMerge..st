checking
warnAboutLosingChangesTo: versionCollection ifCancel: cancelBlock ifMerge: mergeBlock
	
	| result |

	result := (MCMergeOrLoadWarning signalFor: versionCollection).
	
	result = false ifTrue: [ ^ mergeBlock value ].
	result = nil ifTrue: [ ^ cancelBlock value ].
	
	"otherwise we will continue loading"