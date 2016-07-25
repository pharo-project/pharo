accessing
headlessOption
	"Return the default name for the headless option for this VM"
	^ self optionDash, (Smalltalk os isUnix
		ifTrue: [ 'vm-display-null' ]
		ifFalse: [ 'headless' ])