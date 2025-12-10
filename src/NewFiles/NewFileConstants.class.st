"
I hold new file constants.
"
Class {
	#name : 'NewFileConstants',
	#superclass : 'SharedPool',
	#classVars : [
		'NewFileCreationDispositionCreateAlways',
		'NewFileCreationDispositionCreateNew',
		'NewFileCreationDispositionOpenAlways',
		'NewFileCreationDispositionOpenExisting',
		'NewFileCreationDispositionTruncateExisting',
		'NewFileMemMapProtectionReadOnly',
		'NewFileMemMapProtectionReadWrite',
		'NewFileOpenFlagsAppend',
		'NewFileOpenFlagsNone',
		'NewFileOpenModeReadOnly',
		'NewFileOpenModeReadWrite',
		'NewFileOpenModeWriteOnly',
		'NewFileSeekModeCurrent',
		'NewFileSeekModeEnd',
		'NewFileSeekModeSet'
	],
	#category : 'NewFiles',
	#package : 'NewFiles'
}

{ #category : 'class initialization' }
NewFileConstants class >> initialize [
	NewFileOpenModeReadOnly := 0.
	NewFileOpenModeWriteOnly := 1.
	NewFileOpenModeReadWrite := 2.
	
	NewFileOpenFlagsNone := 0.
	NewFileOpenFlagsAppend := 1<<0.

	NewFileSeekModeSet := 0.
	NewFileSeekModeCurrent := 1.
	NewFileSeekModeEnd := 2.
	
	NewFileCreationDispositionCreateNew := 1.
	NewFileCreationDispositionCreateAlways := 2.
	NewFileCreationDispositionOpenExisting := 3.
	NewFileCreationDispositionOpenAlways := 4.
	NewFileCreationDispositionTruncateExisting := 5.
	
	NewFileMemMapProtectionReadOnly := 0.
   NewFileMemMapProtectionReadWrite := 1.

]
