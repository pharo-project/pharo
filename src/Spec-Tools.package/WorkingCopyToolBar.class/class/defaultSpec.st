specs
defaultSpec 

	^ {#ContainerPresenter.
			#add: . {{#model . #packageButton.} . #layout: .  #(FrameLayout
										rightFraction: 0.25
										bottomFraction: 0
										bottomOffset: 25)} . 

			#add: . {{#model . #configButton.} .
						#layout: . #(FrameLayout
										leftFraction: 0.25
										rightFraction: 0.5
										bottomFraction: 0
										bottomOffset: 25) } .
											
			#add: . {{#model . #sliceButton.} .
						#layout: . #(FrameLayout
										leftFraction: 0.5
										rightFraction: 0.75
										bottomFraction: 0
										bottomOffset: 25) } .											

			#add: . {{#model . #browseButton.} .
						#layout: . #(FrameLayout
										leftFraction: 0.75
										bottomFraction: 0
										bottomOffset: 25)} 	
		}