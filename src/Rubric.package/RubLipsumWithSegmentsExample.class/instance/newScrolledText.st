user interface
newScrolledText
	| st r conf |
	st := super newScrolledText.
	conf := RubConfigurationChange new.
	conf
		configurationBlock: [ :scrolledText | 
			scrolledText withTextSegmentIcons.
			scrolledText addSegment: (r := RubUnderlinedSegmentMorph from: 1 to: 29).
			r icon: (self iconNamed: #smallErrorIcon).
			r label: 'Please review'.
			r iconBlock: [ :seg | seg delete ].
			scrolledText addSegment: (r := RubUnderlinedSegmentMorph from: 35 to: 59).
			r icon: (self iconNamed: #smallErrorIcon).
			r label: 'Remove'.
			r
				iconBlock: [ :seg :event | 
					seg textArea selectFrom: seg firstIndex to: seg lastIndex.
					seg textArea replaceSelectionWith: ''.
					seg delete ].
			scrolledText addSegment: (r := RubUnderlinedSegmentMorph from: 430 to: 740).
			r borderColor: Color lightGray.
			r changeable: false.
			r label: 'Edit separately'.
			r iconBlock: [ :seg :event | (seg readWriteView embeddedInMorphicWindowLabeled: 'Lipsum part view') openInHand ].
			r straight: false.
			r icon: (self iconNamed: #smallWarningIcon).
			scrolledText addSegment: (r := RubUnderlinedSegmentMorph from: 810 to: 1000).
			r color: (Color blue alpha: 0.1).
			r borderColor: Color black.
			r label: 'Explore'.
			r changeable: false.
			r icon: (self iconNamed: #smallHelpIcon).
			r iconBlock: [ :seg :event | seg inspect ].
			scrolledText addSegment: (r := RubPlainSegmentMorph from: 1300 to: 1610).
			r label: 'Remove'.
			r
				iconBlock: [ :seg :event | 
					seg textArea selectFrom: seg firstIndex to: seg lastIndex.
					seg textArea replaceSelectionWith: ''.
					seg delete ].
			r color: (Color green alpha: 0.1).
			r borderColor: Color gray.
			r icon: (self iconNamed: #smallWarningIcon) ].
	self textModel announce: conf.
	^ st