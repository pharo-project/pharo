accessing
icon: aForm

	icon image: aForm.
	label 
		ifNotNil: [ icon setBalloonText: label maxLineLength: 100 ].
	action ifNotNil: [ icon on: #click send: #value to: action ]