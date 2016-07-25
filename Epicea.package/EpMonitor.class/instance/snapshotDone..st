announcement handling
snapshotDone: aSnapshotDone
"-> disable this event as a workaround to case 18374"

"	aSnapshotDone isNewImage 
		ifFalse: [ self sessionSnapshot ]"