private
buildLabel: text withIcon: icon

	icon ifNil: [ ^ text
			ifNil: [ '' ]
			ifNotNil: [ text ] ].
	
	^ IconicListItem 
		text: text 
		icon: icon
