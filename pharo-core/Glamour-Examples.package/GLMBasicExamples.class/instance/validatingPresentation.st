updating
validatingPresentation
	<glmBrowser: 'Validating presentation' input: '|collection|
	collection := GLMAnnouncingCollection new.
	collection add: 1; add: 2; add: 3.
	collection'>
	"
	|collection|
	collection := GLMAnnouncingCollection new.
	collection add: 1; add: 2; add: 3.
	self new validatingPresentation openOn: collection."
	
	| browser |
	browser := GLMTabulator new.
	browser column: #one; column: #two.
	browser transmit to: #one; andShow: [:a | 
		a list 
			title: 'List with port validation';
			shouldValidate: true;
			updateOn: GLMItemRemoved from: #yourself;
			updateOn: GLMItemAdded from: #yourself;
			act: [:list :all | all removeLast ] 
				icon: GLMUIThemeExtraIcons glamorousRemove 
				entitled: 'Remove last';
			act: [:list :all | all add: (all size + 1) ] 
				icon: GLMUIThemeExtraIcons glamorousAdd 
				entitled: 'Add last'. 
	 ].
	browser transmit from: #one; to: #two; andShow: [:a | 
		a text title: 'Preview' ].
	^ browser 
