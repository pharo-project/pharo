building
attachExtraBehaviourToPresentationsIn: aComposite

	aComposite presentations do: [ :aPresentation |
		aPresentation addAction: 
			((self closeActionFor: aPresentation)
				condition: [ :currentPresentation |
					self panes size > 1 and: [ 
						self isPresentation: aPresentation inPane: self panes last  ] ] ) ]