menu
addPolyLIneCurveMenuItems: aMenu hand: aHandMorph 

	aMenu addLine;
				addUpdating: #openOrClosePhrase
				target: self
				selector: #toggleOpenOrClosed.
			
			aMenu
				addUpdating: #smoothOrSegmentedPhrase
				target: self
				selector: #toggleSmoothing.