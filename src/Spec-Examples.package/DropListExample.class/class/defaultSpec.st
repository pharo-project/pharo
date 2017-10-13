specs
defaultSpec

	^ { #ContainerPresenter.
			#add:.	{ self topSpec . #layout: . #(#SpecLayoutFrame
													bottomFraction: 0
													bottomOffset: 30) }.
			#add:.	{{#model . #container } . #layout: .  #(#SpecLayoutFrame
													topOffset: 42). }}