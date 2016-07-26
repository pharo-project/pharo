specs
defaultSpec2
^{ #Panel.
		#changeTableLayout.
		#listDirection:. 	#bottomToTop.
		#addMorph:. 		{#model. #listModel.}.
		#addMorph:. 		{#model. #textModel.}.
		#vResizing:.		 #spaceFill.
		#hResizing:.		 #spaceFill.}