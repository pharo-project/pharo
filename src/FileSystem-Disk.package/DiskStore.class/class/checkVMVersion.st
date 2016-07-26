system startup
checkVMVersion
	"Display a warning if the VM is too old"
	| displayError |
	displayError := [ ^ self inform: 'Your VM is too old for this image. Please download the latest VM.' ].
	[(Smalltalk vm interpreterSourceDate > '2012-07-08+2:00' asDate)
		ifFalse: displayError
	] on: Error do: [ :e| displayError value ].