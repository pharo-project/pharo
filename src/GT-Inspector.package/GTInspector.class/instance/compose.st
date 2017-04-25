building
compose
	self title: [:anObject | 'Inspector on ', (self printObjectAsAnItem: anObject)].
	self act: [:b | b update] icon: GLMUIThemeExtraIcons glamorousRefresh entitled: 'Update'.
	self act: [:b | 
		HelpBrowser open selectTopicSatisfying: [:each |
			each owner notNil and:  [
			(each owner key = GTInspectorHelp key) and: [
				each title = 'Overview' ] ] ] ] 
		icon: GLMUIThemeExtraIcons glamorousHelp 
		entitled: 'Help'.
	self act: [:b | b presentationFilter changeFilterRequestFor: b ] entitled: 'Filter presentations'.
	self pager with: [ :browser | 
		browser fixedSizePanes: self class numberOfInspectorPanes.
		browser show: [ :a :each |
			a title: [ self printObjectAsAnItem: each ].
			a dynamicActions: [ :aPresentation | 
				each gtInspectorActions asOrderedCollection ].
			a titleAction: [ :aPresentation | self closeActionFor: aPresentation ].
			a dynamic 
				display: [ :x | 
					(GLMCompositePresentation new with: [:d | 
						each 
							gtInspectorPresentationsIn: d 
							inContext: self ])
							startOn: x ] ] ].
			
	self browser when: GLMPaneAdded send: #actOnPaneAdded: to: self.
	self browser when: GLMPaneRemoved send: #actOnPaneRemoved: to: self.
	
	self class isStepRefreshEnabled ifTrue: [ 
		self wantsAutomaticRefresh: true.
		self wantsSteps: true.
		self stepTime: self class stepRefreshRate ].
	