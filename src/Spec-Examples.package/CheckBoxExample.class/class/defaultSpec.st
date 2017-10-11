specs
defaultSpec

	^ { #ContainerPresenter.
			#add:.	{ self topSpec. #layout:. 	#(#SpecLayoutFrame
													bottomFraction: 0
													bottomOffset: 20) }.
			#add:.	{{#model . #container } . #layout: . #(#SpecLayoutFrame topOffset: 22) }
	   }