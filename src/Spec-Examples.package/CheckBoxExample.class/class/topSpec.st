specs
topSpec

	^ { #ContainerPresenter.
			#add:. {{#model .	#button1 } . #layout:.  #(#SpecLayoutFrame
															rightFraction: 0.33
															bottomFraction: 0
															bottomOffset: 25)}.
			#add:.  {{#model .	#button2 } . #layout:. #(#SpecLayoutFrame
															leftFraction: 0.33
															rightFraction: 0.66
															bottomFraction: 0
															bottomOffset: 25)}.
			#add:.  {{#model .	#button3 } . #layout:. #(#SpecLayoutFrame
															leftFraction: 0.66
															bottomFraction: 0
															bottomOffset: 25)}}