tests
compareLayout: aLayout with: anOtherLayout
	"I am here because when I create this method there is no equals method in LayoutFrame. Only pointers equality."

	^ aLayout leftFraction = anOtherLayout leftFraction
		and:
			[ aLayout rightFraction = anOtherLayout rightFraction
				and:
					[ aLayout topFraction = anOtherLayout topFraction
						and:
							[ aLayout bottomOffset = anOtherLayout bottomOffset
								and:
									[ aLayout leftOffset = anOtherLayout leftOffset
										and:
											[ aLayout rightOffset = anOtherLayout rightOffset
												and:
													[ aLayout topOffset = anOtherLayout topOffset
														and: [ aLayout bottomOffset = anOtherLayout bottomOffset ] ] ] ] ] ] ]