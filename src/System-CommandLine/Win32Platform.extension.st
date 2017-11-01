Extension { #name : #Win32Platform }

{ #category : #'*System-CommandLine' }
Win32Platform >> newTerminfo [
	"Windows does not support the termCap"
	^ NullTermInfo new
]
