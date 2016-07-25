private
installActionsOnModel: aMorphicModel fromPresentation: aPresentation 
	aMorphicModel when: GLMMenuItemSelected do: [ :ann | ann action morphicActOn: aPresentation ].
	aMorphicModel when: GLMKeyStroke do: [ :ann | ann action actOn: aPresentation ].